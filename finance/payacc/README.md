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

## Connect to QuickBooks

Point your browser to `http://localhost:3013/accounting/connection`, enter your QuickBooks credentials.  The app will respond with a success message.


# Development

## Tasks

✅ receive events from Stripe
✅ send to in-service queue
✅ use NestJS task scheduling to send to accounting wrapper
send to QuickBooks

