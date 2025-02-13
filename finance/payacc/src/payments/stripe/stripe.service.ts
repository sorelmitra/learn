import { HttpException, HttpStatus, Injectable, Logger } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import Stripe from 'stripe';
import {
  CreatePaymentInput,
  Payment,
  PaymentMethod,
  PaymentMethodComboInput,
  PaymentStatusName,
  UpdatePaymentInput,
} from '../dto/payments.dto';
import { PaymentsProcessor, PaymentsProcessorName } from '../processor/payments-processor';
import {
  getStripePaymentMethods,
  getStripePaymentMethodTypeMappings,
  getStripeStatusMappings,
  StripePaymentMethodCombo as StripePaymentMethodCombo,
} from './stripe-mappings';
import { Customer, CustomerInput } from '../dto/customers.dto';
import { makeId } from 'src/common/utils/utils';

@Injectable()
export class StripeService implements PaymentsProcessor {
  private stripe: Stripe;
  private stripePaymentMethods = getStripePaymentMethods();
  private stripeStatusMappings = getStripeStatusMappings();
  private stripePaymentMethodTypeMappings = getStripePaymentMethodTypeMappings();

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
    const customer = await this.ensureCustomerExists(input.customer);
    const stripePaymentIntentInput: Stripe.PaymentIntentCreateParams = {
      amount: this.toStripeInt(input.amount),
      currency: 'USD',
      capture_method: 'automatic',
      customer: customer.id,
      ...this.toStripePaymentMethodCombo(input.methodCombo),
    };
    this.logger.debug('Stripe create payment intent input', stripePaymentIntentInput);
    const response = await this.stripe.paymentIntents.create(stripePaymentIntentInput);
    this.logger.debug('Stripe create payment intent response', response);
    return this.mapStripePaymentIntentToPayment(response);
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
      ...this.toStripePaymentMethodCombo(input.methodCombo),
    });
    this.logger.debug(`Stripe update payment intent ${processorId} response`, response);
    return this.mapStripePaymentIntentToPayment(response);
  }

  async getPayment(processorId: string): Promise<Payment> {
    const response = await this.stripe.paymentIntents.retrieve(processorId);
    this.logger.debug(`Stripe get payment intent ${processorId} response`, response);
    return this.mapStripePaymentIntentToPayment(response);
  }

  async confirmPayment(processorId: string): Promise<Payment> {
    const response = await this.stripe.paymentIntents.confirm(processorId);
    this.logger.debug(`Stripe confirm payment intent ${processorId} response`, response);
    return this.mapStripePaymentIntentToPayment(response);
  }

  private async ensureCustomerExists(customer: CustomerInput) {
    const existingCustomer = await this.findCustomer(customer);
    if (existingCustomer) return existingCustomer;

    return await this.stripe.customers.create(this.mapCustomerToStripe(customer));
  }

  private async findCustomer(customer: CustomerInput) {
    const response = await this.stripe.customers.search({ query: `email: "${customer.email}"` });
    if (response?.data?.length > 1) {
      const errorMessage = `Found more than one customers for ${customer.email}`;
      this.logger.error(errorMessage, response.data);
      throw new HttpException(errorMessage, HttpStatus.BAD_REQUEST);
    }
    if (!response?.data?.length) {
      return undefined;
    }
    return response.data[0];
  }

  private mapCustomerToStripe(customer: CustomerInput): Stripe.CustomerCreateParams | undefined {
    return {
      email: customer.email,
      name: customer.name,
    };
  }

  async mapStripePaymentIntentToPayment(
    stripePaymentIntent: Stripe.PaymentIntent,
  ): Promise<Payment> {
    return {
      id: this.makeId(stripePaymentIntent.id),
      amount: this.fromStripeInt(stripePaymentIntent.amount),
      status: this.mapStripeStatus(stripePaymentIntent.status),
      customer: await this.mapStripeCustomer(stripePaymentIntent.customer),
      method: await this.getPaymentMethod(stripePaymentIntent.payment_method),
    };
  }

  private async getPaymentMethod(
    paymentMethodResponse: string | Stripe.PaymentMethod | null,
  ): Promise<PaymentMethod | undefined> {
    if (!paymentMethodResponse) return undefined;
    if (typeof paymentMethodResponse === 'object') {
      return this.mapStripePaymentMethod(paymentMethodResponse);
    }
    const stripePaymentMethod = await this.stripe.paymentMethods.retrieve(paymentMethodResponse);
    return this.mapStripePaymentMethod(stripePaymentMethod);
  }

  private mapStripePaymentMethod(stripePaymentMethod: Stripe.PaymentMethod): PaymentMethod {
    const paymentMethodType = this.stripePaymentMethodTypeMappings.get(stripePaymentMethod.type);
    if (!paymentMethodType) {
      throw new HttpException(
        `Unmapped payment method type ${paymentMethodType}`,
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
    this.logger.debug(
      `Mapping Stripe payment method ${stripePaymentMethod.id} from`,
      stripePaymentMethod,
    );
    const details = stripePaymentMethod[stripePaymentMethod.type];
    return {
      type: paymentMethodType,
      bankName: details?.['bank_name'],
      lastFour: details?.['last4'],
      routingNumber: details?.['routing_number'],
    };
  }

  private async mapStripeCustomer(
    stripeCustomerResponse: string | Stripe.Customer | Stripe.DeletedCustomer | null,
  ): Promise<Customer | undefined> {
    let stripeCustomer: Stripe.Customer;
    if (!stripeCustomerResponse) return undefined;
    if (typeof stripeCustomerResponse === 'object') {
      stripeCustomer = stripeCustomerResponse as Stripe.Customer;
    } else {
      stripeCustomer = (await this.stripe.customers.retrieve(
        stripeCustomerResponse,
      )) as Stripe.Customer;
    }

    return {
      id: this.makeId(stripeCustomer.id),
      email: stripeCustomer.email ?? 'missing email',
      name: stripeCustomer.name ?? 'missing name',
    };
  }

  private mapStripeStatus(stripeStatus: string): PaymentStatusName {
    const status = this.stripeStatusMappings.get(stripeStatus);
    if (!status) {
      throw new HttpException(`Unknown Stripe status ${stripeStatus}`, HttpStatus.BAD_REQUEST);
    }
    return status;
  }

  makeId(id: string): string {
    return makeId(PaymentsProcessorName.STRIPE)(id);
  }

  private toStripePaymentMethodCombo(
    paymentMethod?: PaymentMethodComboInput,
  ): StripePaymentMethodCombo {
    if (!paymentMethod) return {};
    const mapping = this.stripePaymentMethods.get(paymentMethod);
    if (!mapping) {
      throw new HttpException(`Unknown payment method ${paymentMethod}`, HttpStatus.BAD_REQUEST);
    }
    return mapping;
  }

  private fromStripeInt(amount: number) {
    return Number(amount / 100);
  }

  private toStripeInt(amount: number) {
    return Number(Math.round(amount * 100).toFixed(0));
  }
}
