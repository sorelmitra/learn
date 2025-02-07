import { StripeEvent } from 'src/payments/dto/stripe.dto';
import { PaymentEvent } from 'src/payments/events/payments-events';
import Stripe from 'stripe';
import { StripeService } from '../stripe.service';
import { Payment } from 'src/payments/dto/payments.dto';
import { Injectable } from '@nestjs/common';

@Injectable()
export abstract class StripeEventHandler {
  constructor(protected readonly stripeService: StripeService) {}

  abstract handle(event: StripeEvent): Promise<PaymentEvent>;

  async getPaymentFromStripePaymentIntent(
    stripePaymentIntent: Stripe.PaymentIntent,
  ): Promise<Payment> {
    return this.stripeService.mapStripePaymentIntentToPayment(stripePaymentIntent);
  }

  toPaymentEvent({
    stripeEvent,
    payment,
  }: {
    stripeEvent: StripeEvent;
    payment: Payment;
  }): Omit<PaymentEvent, 'name'> {
    return {
      id: this.stripeService.makeId(stripeEvent.id),
      created: new Date(stripeEvent.created * 1000),
      amount: payment.amount,
      method: payment.method,
      customer: payment.customer,
    };
  }
}
