Automatic Acceptance Testing with External Mocking
==================================================

Definitions:

- _navigation service_: A service under test.
- _boat_: An external dependency service of the service under test.

# Table of Contents

<!-- toc -->

- [Mocking Framework](#mocking-framework)
- [Running AATs](#running-aats)
- [AAT with Mocking](#aat-with-mocking)
  * [1. Write the Test](#1-write-the-test)
  * [2. Identify What to Mock](#2-identify-what-to-mock)
  * [3. Write the Test that calls to the Mock](#3-write-the-test-that-calls-to-the-mock)
  * [4. Write the Mock](#4-write-the-mock)

<!-- tocstop -->

# Mocking Framework

[Mocking Framework](aat-mocking.md)

# Running AATs

1. Start the mocks server: `sudo yarn aat-mocks-controller`

2. Run the tests: `yart aat-local`

# AAT with Mocking

This guide will show AATs with mocking.

## 1. Write the Test

Add a new file to the `aat/features` directory.  Name it `Boat.feature`.

  ```gherkin
  Feature: Boat
    Scenario Outline: General Conditions
      Given I have a boat <condition>
      When I <action>
      Then It should <result>
    Examples:
      | condition    | action         | result   |
      | moored       | untie boat     | drift    |
  ```

## 2. Identify What to Mock

Suppose the navigation service calls to this endpoint in order to untie:

- `boat.chatham.medway/api/untie`

## 3. Write the Test that calls to the Mock

Now write your test.  Notice how we just call to the DSL here, and that's the recommended course of action.  Add a file `boat.ts` into `aat/step-definitions`.

```typescript
const { runMockOp } = coolMocksOperationRunner();

Given(/^I have a boat (\w+)$/, async (condition: BoatCondition) => {
    await runMockOp(boatMockName(), CoolOperationName.SET_EXPECTED_INPUT, {
      payload: { condition }
    });
});

When(/^I (\w+)$/, async (action: BoatAction) => {
  // ... tell navigation service to untie here
});

Then(/^It should (\w+)$/, async (result: BoatResult) => {
  // ... check the boat drifts
});
```

## 4. Write the Mock

We work around replacing real URLs for the external services that the navigation service calls.

Add the **real service domain name** to the DNS mocking configuration, i.e. this line to `/etc/hosts`:

    127.0.0.1 boat.chatham.medway

Now we can proceed to adding the mock.

**Step 1**: Put the boat name in `aat/mocks/names.ts`:

```typescript
export const boatMockName = () => 'Boat Mock';
```

This is used in the test as you saw above.

**Step 2**: Add the mock to `aat/mocks/boatMock.ts`:

Here we need to implement our handler:

- `handle`: Handles the call from the navigation service, return the mocked response.  Uses fixtures from `aat/fixtures/boat`.  We assume here that the service sends the boat condition "moored" as input.
- `routes`: The routes that the mock can handle.  The mocking framework automatically add athem to the Hapi server.  We've just one here.

We also need to implement the `CoolMockOpTarget` type, which is used by the framework to know which mock to call.

- `setExpectedInput`: The test calls it via the controller to set the expected input when the mock is called by our navigation service.
- `setDesiredResult`: The test calls it via the controller to set the desired result when the mock is called by our navigation service.  Not shown here.
- `name`: Name of the mock, used by the framework to identify the mock and to register it with `runMockOp`.

In `aat/mocks` add a file `boatMock.ts`:

```typescript
import { boatMockName } from './names';

const defaultboatMock = () => {
  const name = boatMockName();
  let expectedCondition: string;
  const { fixture } = useFixtures('boat');

  const setExpectedInput = (input?: CoolOperationInput) =>
    (expectedCondition = input?.payload?.condition as string);

  const setDesiredResult = () => {};

  const handle = (request: Hapi.Request) => {
    const error = checkInputAsExpected(request, name, expectedCondition);
    if (error) {
      return error;
    }
    const response = JSON.parse(fixture('untieMockResponse')) as Record<string, any>;
    response.id = expectedCondition;
    return { body: JSON.stringify(response) };
  };

  const routes = [
    {
      method: 'POST',
      path: 'api/untie',
      handler: handle
    },
  ];


  return { setExpectedInput, setDesiredResult, name, routes };
};

const boatMockInstance = defaultboatMock();

export const boatMock = () => {
  return boatMockInstance;
};
```

**Step 3**: Add the mock to `aat/mocks/index.ts`:

```typescript
export const mocks = [
  // ...
  boatMock
];
```

The framework imports this file and for each mock it finds, it:

1. Adds its routes to the server.
2. Registers it for communications via `runMockOp`.

**How does all the above work**:

1. The navigation service calls `https://boat.chatham.medway/api/untie`.
2. The TCP/IP stack makes DNS request for `boat.chatham.medway`.
3. The hosts file comes up with `127.0.0.1`.
4. The request made from the navigation service goes to `https://127.0.0.1/api/untie`.
5. The mocks controller, listens on `127.0.0.1:443`, so it gets that request.
6. This hits the `untie` route we have defined above.
7. The Hapi.dev server calls to the `handle` function we have defined in `boatMock.ts`.
8. Finally, the `handle` function returns the mocked response.

