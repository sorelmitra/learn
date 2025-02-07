import { HttpException, HttpStatus, Injectable, Logger } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import Stripe from 'stripe';
import {
  CreatePaymentInput,
  Payment,
  PaymentStatusName,
  UpdatePaymentInput,
} from '../dto/payments.dto';
import {
  makeId,
  PaymentMethodName,
  PaymentsProcessor,
  PaymentsProcessorName,
} from '../processor/payments-processor';
import {
  getStripePaymentMethods,
  getStripeStatusMappings,
  PaymentMethodMapping,
} from './stripe-mappings';

@Injectable()
export class StripeService implements PaymentsProcessor {
  private stripe: Stripe;
  private stripePaymentMethods = getStripePaymentMethods();
  private stripeStatusMappings = getStripeStatusMappings();

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
      currency: 'USD',
      capture_method: 'automatic',
      ...this.toStripePaymentMethod(input.method),
    };
    this.logger.debug('Stripe create payment intent input', stripePaymentIntentInput);
    const response = await this.stripe.paymentIntents.create(stripePaymentIntentInput);
    this.logger.debug('Stripe create payment intent response', response);
    return this.mapStripeResponseToPayment(response);
  }

  async updatePayment({
    processorId,
    input,
  }: {
    processorId: string;
    input: UpdatePaymentInput;
  }): Promise<Payment> {
    const response = await this.stripe.paymentIntents.update(processorId, {
      amount: input.amount ? this.toStripeInt(input.amount) : undefined,
      ...this.toStripePaymentMethod(input.method),
    });
    this.logger.debug(`Stripe update payment intent ${processorId} response`, response);
    return this.mapStripeResponseToPayment(response);
  }

  async getPayment(processorId: string): Promise<Payment> {
    const response = await this.stripe.paymentIntents.retrieve(processorId);
    this.logger.debug(`Stripe get payment intent ${processorId} response`, response);
    return this.mapStripeResponseToPayment(response);
  }

  async confirmPayment(processorId: string): Promise<Payment> {
    const response = await this.stripe.paymentIntents.confirm(processorId);
    this.logger.debug(`Stripe confirm payment intent ${processorId} response`, response);
    return this.mapStripeResponseToPayment(response);
  }

  private mapStripeResponseToPayment(response: Stripe.Response<Stripe.PaymentIntent>): Payment {
    return {
      id: this.makeId(response.id),
      status: this.mapStripeStatus(response.status),
    };
  }

  private mapStripeStatus(stripeStatus: string): PaymentStatusName {
    const status = this.stripeStatusMappings.get(stripeStatus);
    if (!status) {
      throw new HttpException(`Unknown Stripe status ${stripeStatus}`, HttpStatus.BAD_REQUEST);
    }
    return status;
  }

  private makeId(id: string): string {
    return makeId(PaymentsProcessorName.STRIPE)(id);
  }

  private toStripePaymentMethod(paymentMethod?: PaymentMethodName): PaymentMethodMapping {
    if (!paymentMethod) return {};
    const mapping = this.stripePaymentMethods.get(paymentMethod);
    if (!mapping) {
      throw new HttpException(`Unknown payment method ${paymentMethod}`, HttpStatus.BAD_REQUEST);
    }
    return mapping;
  }

  private toStripeInt(amount: number) {
    return Number(Math.round(amount * 100).toFixed(0));
  }
}
