Automatic Acceptance Testing with External Mocking
==================================================

Definitions:

- _drawing service_: A service under test.
- _pencil_: An external dependency service of the service under test.

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

# Running AATs

1. Start the mocks server: `sudo yarn aat-mocks-controller`

2. Run the tests: `yart aat-local`

# AAT with Mocking

We will be mocking the pencil dependency without touching the drawing service code.

## 1. Write the Test

Add a new file to the `aat/features` directory.  Name it `Pencil.feature`.

  ```gherkin
  Feature: Pencil
    Scenario Outline: Lines
      Given I have a pencil of <color>
      When I draw a line
      Then the line should appear in <color>
    Examples:
      | color    |
      | black    |
      | red      |
  ```

## 2. Identify What to Mock

Suppose the drawing service calls to this endpoint in order to set the pencil's color:

- `pencil.factory.com/api/apply-color`

## 3. Write the Test that calls to the Mock

Now write your test.  Notice how we just call to the DSL here, and that's the recommended course of action.  Add a file `pencil.ts` into `aat/step-definitions`.

```typescript
const { tellMock } = coolMocksCommunicator();

Given(/^I have a pencil of (\w+)$/, async (color: PencilColor) => {
    await tellMock(pencilMockName(), CoolOperationName.SET_EXPECTED_INPUT, {
      payload: { color }
    });
});

When(/^I draw a line$/, async (action: PencilAction) => {
  // ... tell drawing service to draw here
});

Then(/^the line should appear in (\w+)$/, async (result: PencilResult) => {
  // ... check the line result
});
```

## 4. Write the Mock

We work around replacing real URLs for the external services that the drawing service calls.

Add the **real service domain name** to the DNS mocking configuration, i.e. this line to `/etc/hosts`:

    127.0.0.1 pencil.factory.com

Now we can proceed to adding the mock.

**Step 1**: Put the pencil name in `aat/mocks/names.ts`:

```typescript
export const pencilMockName = () => 'Pencil Mock';
```

This is used in the test as you saw above.

**Step 2**: Add the mock to `aat/mocks/pencilMock.ts`:

Here we need to implement our handler:

- `handle`: Handles the call from the drawing service, return the mocked response.  Uses fixtures from `aat/fixtures/pencil`.  We assume here that the service sends the pencil color as input.
- `routes`: The routes that the mock can handle.  The mocking framework automatically adds them to the Hapi server.

We also need to implement the `CoolMockOpTarget` type, which is used by the framework to know which mock to call.

- `setExpectedInput`: The test calls it via the controller to set the expected input when the mock is called by our drawing service.
- `setDesiredResult`: The test calls it via the controller to set the desired result when the mock is called by our drawing service.  Not shown here.
- `name`: Name of the mock, used by the framework to identify the mock and to register it with `tellMock`.

In `aat/mocks` add a file `pencilMock.ts`:

```typescript
import { pencilMockName } from './names';

const defaultpencilMock = () => {
  const name = pencilMockName();
  let expectedColor: string;
  const { fixture } = useFixtures('pencil');

  const setExpectedInput = (input?: CoolOperationInput) =>
    (expectedColor = input?.payload?.color as string);

  const setDesiredResult = () => {};

  const handle = (request: Hapi.Request) => {
    const error = checkInputIsAsExpected(request, name, expectedColor);
    if (error) {
      return error;
    }
    const response = JSON.parse(fixture('setColorMockResponse')) as Record<string, any>;
    response.id = expectedColor;
    return { body: JSON.stringify(response) };
  };

  const routes = [
    {
      method: 'POST',
      path: 'api/apply-color',
      handler: handle
    },
  ];


  return { setExpectedInput, setDesiredResult, name, routes };
};

const pencilMockInstance = defaultpencilMock();

export const pencilMock = () => {
  return pencilMockInstance;
};
```

**Step 3**: Add the mock to `aat/mocks/index.ts`:

```typescript
export const mocks = [
  // ...
  pencilMock
];
```

The framework imports this file and for each mock it finds, it:

1. Adds its routes to the server.
2. Registers it for communications via `tellMock`.

**How does all the above work**:

To mock a dependency of our service, we:

- Hijack the route to that dependency.
- Install a mock server that listens on the hijacked route.
- Control the mock server's behavior from inside the tests.

So:

1. The drawing service calls `https://pencil.factory.com/api/apply-color`.
2. The TCP/IP stack makes DNS request for `pencil.factory.com`.
3. The hosts file comes up with `127.0.0.1`.
4. The request made from the drawing service goes to `https://127.0.0.1/api/apply-color`.
5. The mocks controller listens on `127.0.0.1:443`, so it gets that request.
6. This hits our `apply-color` route.
7. The Hapi server calls the `handle` function we have defined in `pencilMock.ts`.
8. Finally, the `handle` function returns the mocked response.

