import { HttpException, HttpStatus, Injectable, Logger } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import Stripe from 'stripe';
import { CreatePaymentInput, Payment } from '../dto/payments.dto';
import { PaymentsProcessor } from '../processor/payments-processor';

@Injectable()
export class StripeService implements PaymentsProcessor {
  private stripe: Stripe;

  constructor(
    private readonly logger: Logger,
    private readonly configService: ConfigService,
  ) {
    const stripeKey = this.configService.get<string>('STRIPE_KEY');
    if (!stripeKey) {
      throw new HttpException(
        `Missing Stripe API Key in config!`,
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
    this.logger.debug(`Stripe API Key ${stripeKey}`);
    this.stripe = new Stripe(stripeKey);
    this.logger.log(`Stripe client created`);
  }

  async createPayment(input: CreatePaymentInput): Promise<Payment> {
    const stripePaymentIntentInput: Stripe.PaymentIntentCreateParams = {
      amount: this.toStripeInt(input.amount),
      currency: 'RON',
    };
    this.logger.debug(
      'Stripe create payment intent input',
      stripePaymentIntentInput,
    );
    const response = await this.stripe.paymentIntents.create(
      stripePaymentIntentInput,
    );
    this.logger.debug('Stripe create payment intent response', response);
    return { id: response.id };
  }

  private toStripeInt(amount: number) {
    return Number(Math.round(amount * 100).toFixed(0));
  }
}
