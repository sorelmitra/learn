import { Module } from '@nestjs/common';
import { StripeService } from './stripe.service';
import { CommonModule } from 'src/common/common.module';
import { StripeWebhookService } from './webhook/stripe-webhook.service';
import { StripeEventHandlerFactory } from './events/stripe-event-handler.factory';
import { StripeEventHandlerPaymentIntentCreated } from './events/stripe-event-handler-payment-intent-created';
import { StripeEventHandlerPaymentIntentFailed } from './events/stripe-event-handler-payment-intent-failed';
import { StripeEventHandlerPaymentIntentSucceeded } from './events/stripe-event-handler-payment-intent-succeeded';
import { QueueModule } from 'src/common/queue/queue.module';

@Module({
  imports: [CommonModule, QueueModule],
  providers: [
    StripeService,
    StripeWebhookService,
    StripeEventHandlerFactory,
    StripeEventHandlerPaymentIntentCreated,
    StripeEventHandlerPaymentIntentFailed,
    StripeEventHandlerPaymentIntentSucceeded,
  ],
  exports: [
    StripeService,
    StripeWebhookService,
    StripeEventHandlerFactory,
    StripeEventHandlerPaymentIntentCreated,
    StripeEventHandlerPaymentIntentFailed,
    StripeEventHandlerPaymentIntentSucceeded,
  ],
})
export class StripeModule {}
