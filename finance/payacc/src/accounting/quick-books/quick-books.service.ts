import { Injectable } from '@nestjs/common';
import { PaymentEvent } from 'src/payments/events/payments-events';
import { AccountingProcessor } from '../accounting-processor/accounting-processor';

@Injectable()
export class QuickBooksService extends AccountingProcessor {
  constructor() {
    super();
  }

  async handlePaymentCreated(event: PaymentEvent): Promise<void> {
    throw new Error('Method not implemented.');
  }

  async handlePaymentSucceeded(event: PaymentEvent): Promise<void> {
    throw new Error('Method not implemented.');
  }

  async handlePaymentFailed(event: PaymentEvent): Promise<void> {
    throw new Error('Method not implemented.');
  }
}
