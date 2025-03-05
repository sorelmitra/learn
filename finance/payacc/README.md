## Description

Sample project for integrating with various payments and accounting providers.

Developed using the [Nest](https://github.com/nestjs/nest) framework TypeScript starter repository.

## Project setup

Add a `.dev.env` file in the root of the project, with this content:

```env
STRIPE_KEY=<Stripe key obtained from your stripe setup>
```

To connect using a different Stripe key, you can create another env file, say `.other.dev`, create a `package.json` script named `start:other`, and then make sure to set `ENV=other` in this script.  Similarly, create a `start:other:stripe:events:local-forwarding` script, and make sure to run this one instead of the `dev` one.

If your Stripe configuration uses [Stripe Connect](https://docs.stripe.com/connect), you can specify the following extra key in your `.*.env` files:

```env
STRIPE_CONNECTED_ACCOUNT_ID=<Stripe connected account ID>
```

**Note**: While the service does send `on_behalf_of` and `transfer_data.destination` if you pass in `STRIPE_CONNECTED_ACCOUNT_ID`, the payment intent created this cannot be seen on the target Stripe connected account, for reasons that I have not yet found.

```bash
$ yarn install
```

## Run the project

First, start Stripe events forwarding:

```bash
yarn run start:dev:stripe:events:local-forwarding
```

Now run:

```bash
# development
$ yarn run start

# development watch mode
$ yarn run start:dev

# production mode
$ yarn run start:prod
```

## Configuration

Create a file `.dev.env` with this content:

```shell
STRIPE_KEY=<Stripe Test-Key>

QB_CLIENT_ID=<QuickBooks Client-Id>
QB_CLIENT_SECRET=<QuickBooks Client-Secret>
QB_ENVIRONMENT=sandbox  # Use 'production' for live data
QB_TOKEN_URL=https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer
QB_REDIRECT_URI=http://localhost:3013/oauth/callback/intuit  # Set this in Intuit -> <My App> -> Settings -> Redirect URIs
INTUIT_V3_ENDPOINT_BASE_URL=https://sandbox-quickbooks.api.intuit.com/v3/company/
```

## Stripe Connect and Operation

The app will automatically connect to Stripe given the correct `STRIPE_KEY` is set in the env file.

## Start the App

First start the events local forwarding:

```shell
STRIPE_KEY=sk_test_51Qp98nFwcFop6SdQx0W3CO7zIGDhhVM5hWEhNEFzPzeFQ1U1IMKAtKRGd9486PTd9L9ctk0594IAawsqUSv0dABN0035BfOj1N && stripe listen --api-key ${STRIPE_KEY} --forward-to http://localhost:3013/webhook/stripe
```

Now start the app: `npm run start:dev`.

### Scenario: Failed Payment

To create a payment that fails:

```shell
curl -X POST -H "Content-Type: application/json" http://127.0.0.1:3013/payments -d '{ "methodCombo": "AchNotAuthorized", "customer": {"email": "orc@dark.net", "name": "Lokthar" }, "amount": 16 }'
```

The response will include an `id` of the form `stripe_pi_XXX`.  Take note of this ID, as it will be used in subsequent operations.

To attempt to pay (confirm) the payment:

```shell
curl -X PATCH -H "Content-Type: application/json" http://127.0.0.1:3013/payments/stripe_pi_XXX/confirm
```

The payment will have a status of `Pending`.  Later on, the event `payment_intent.payment_failed` will come into the app.

To get the payment:

```shell
curl -X GET -H "Content-Type: application/json" http://127.0.0.1:3013/payments/stripe_pi_XXX
```

Now the payment will have a status of `Open` because payment has failed.

To update the payment with a payment method that succeeds, do this:

```shell
curl -X PATCH -H "Content-Type: application/json" http://127.0.0.1:3013/payments/stripe_pi_XXX -d '{"methodCombo": "AchSuccess", "amount": 17}
```

Now you can re-attempt to pay it.

### Scenario: Succeeded Payment

To create a payment that succeeds:

```shell
curl -X POST -H "Content-Type: application/json" http://127.0.0.1:3013/payments -d '{ "methodCombo": "AchSuccess", "customer": {"email": "orc@dark.net", "name": "Lokthar" }, "amount": 18 }'
```

Now attempt to pay it.  Status should become `Succeeded` after a while.

## QuickBooks Connect and Operation

### Connect to QuickBooks

Steps:

A) The initial connection to Intuit requires user consent and is done via the browser.

Point your browser to `http://localhost:3013/accounting/connection`, enter your QuickBooks credentials.  The app will respond with a success message.

B) Upon connection the app gets back from Intuit: a realm ID, a short-term access token (1h), and a long-term refresh token (100 days).  All this information is stored in `.dev-cache.json`.  The access token will be fetched from this cache, and if expired it will be obtained again by the app using the refresh token.

C) When the refresh token expires the app will no longer be able to connect into QuickBooks.  Because of this, there needs to be a persistent storage of un-synced items (not implemented).  The app needs to periodically check the refresh token, and when it works, it should re-sync the failed items.

In order to obtain a new refresh token, the user will have to go back to step A.

### Record a Payment (Partially Implemented)

Create a payment and bring it into the `Succeeded` state.  You should see in the app a log message stating `Sending payment event PaymentSucceeded / stripe_evt_ZZZ to consumers`.

At this stage of the implementation, recording a payment fails with these messages:

- `Api call to QuickBooks path /payment failed`
- `Detail: 'Required parameter CustomerRef is missing in the request'`

We have not went further with this, because things become more complicated with the need of managing customers and possibly other QuickBooks elements.

# Development

## Tasks

✅ receive events from Stripe
✅ send to in-service queue
✅ use NestJS task scheduling to send to accounting wrapper
send to QuickBooks

