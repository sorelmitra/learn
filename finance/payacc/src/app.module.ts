import { Logger, Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { ConfigModule, ConfigService } from '@nestjs/config';
import { PaymentsProcessorService } from './payments/processor/payments-processor.service';
import { StripeService } from './payments/stripe/stripe.service';
import { StripeWebhookService } from './payments/stripe/webhook/stripe-webhook.service';
import { QueueService } from './common/queue/queue.service';
import { StripeEventHandlerFactory } from './payments/stripe/events/stripe-event-handler-factory';
import { StripeEventHandlerPaymentIntentCreated } from './payments/stripe/events/stripe-event-handler-payment-intent-created';
import { StripeEventHandlerPaymentIntentFailed } from './payments/stripe/events/stripe-event-handler-payment-intent-failed';
import { StripeEventHandlerPaymentIntentSucceeded } from './payments/stripe/events/stripe-event-handler-payment-intent-succeeded';

@Module({
  imports: [
    ConfigModule.forRoot({
      envFilePath: `.${process.env.ENV}.env`,
    }),
  ],
  controllers: [AppController],
  providers: [
    AppService,
    Logger,
    ConfigService,
    PaymentsProcessorService,
    StripeService,
    StripeWebhookService,
    StripeEventHandlerFactory,
    StripeEventHandlerPaymentIntentCreated,
    StripeEventHandlerPaymentIntentFailed,
    StripeEventHandlerPaymentIntentSucceeded,
    QueueService,
  ],
})
export class AppModule {}
