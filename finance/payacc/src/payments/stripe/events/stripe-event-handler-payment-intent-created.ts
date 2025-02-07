import { StripeEvent } from 'src/payments/dto/stripe.dto';
import { StripeEventHandler } from './stripe-event-handler';
import { PaymentEvent, PaymentEventName } from 'src/payments/events/payments-events';
import Stripe from 'stripe';

export class StripeEventHandlerPaymentIntentCreated extends StripeEventHandler {
  async handle(stripeEvent: StripeEvent): Promise<PaymentEvent> {
    const payment = await this.getPaymentFromStripePaymentIntent(
      stripeEvent.data.object as Stripe.PaymentIntent,
    );
    return {
      ...this.toPaymentEvent({ stripeEvent, payment }),
      name: PaymentEventName.PaymentCreated,
    };
  }
}
