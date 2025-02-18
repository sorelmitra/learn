import { Injectable } from '@nestjs/common';
import { PaymentEvent } from 'src/payments/events/payments-events';
import { AccountingProcessor } from '../accounting-processor/accounting-processor';

@Injectable()
export class QuickBooksService extends AccountingProcessor {
  handlePaymentFailed(event: PaymentEvent): Promise<void> {
    throw new Error('Method not implemented.');
  }
  handlePaymentSucceeded(event: PaymentEvent): Promise<void> {
    throw new Error('Method not implemented.');
  }
  handlePaymentCreated(event: PaymentEvent): Promise<void> {
    throw new Error('Method not implemented.');
  }
}
