import { Injectable, Logger } from '@nestjs/common';
import { StripeEventHandler } from './stripe-event-handler';
import { StripeEventHandlerPaymentIntentCreated } from './stripe-event-handler-payment-intent-created';
import { StripeEventHandlerPaymentIntentFailed } from './stripe-event-handler-payment-intent-failed';
import { StripeEventHandlerPaymentIntentSucceeded } from './stripe-event-handler-payment-intent-succeeded';

@Injectable()
export class StripeEventHandlerFactory {
  constructor(
    private readonly logger: Logger,
    private readonly stripeEventHandlerPaymentIntentCreated: StripeEventHandlerPaymentIntentCreated,
    private readonly stripeEventHandlerPaymentIntentFailed: StripeEventHandlerPaymentIntentFailed,
    private readonly stripeEventHandlerPaymentIntentSucceeded: StripeEventHandlerPaymentIntentSucceeded,
  ) {}

  private eventHandlersMapping = new Map<string, StripeEventHandler>([
    ['payment_intent.created', this.stripeEventHandlerPaymentIntentCreated],
    ['payment_intent.payment_failed', this.stripeEventHandlerPaymentIntentFailed],
    ['payment_intent.succeeded', this.stripeEventHandlerPaymentIntentSucceeded],
  ]);

  create(stripeEventType: string): StripeEventHandler | undefined {
    const handler = this.eventHandlersMapping.get(stripeEventType);
    if (!handler) {
      this.logger.debug(`Ignoring Stripe event ${stripeEventType}, as no handler was found for it`);
    }
    return handler;
  }
}
