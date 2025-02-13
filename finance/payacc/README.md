## Description

Sample project for integrating with various payments and accounting providers.

Developed using the [Nest](https://github.com/nestjs/nest) framework TypeScript starter repository.

## Project setup

Add a `.<ENV>.env` file in the root of the project, e.g. `.dev.env`, with this content:

```env
STRIPE_KEY=<Stripe key obtained from your stripe account>
```

```bash
$ yarn install
```

## Run the project

First, start Stripe events forwarding:

```bash
yarn run start:stripe:events:local-forwarding
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
