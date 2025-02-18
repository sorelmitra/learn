import { PaymentEvent } from 'src/payments/events/payments-events';

export enum AccountingProcessorName {
  QUICK_BOOKS = 'QuickBooks',
}

export abstract class AccountingProcessor {
  abstract handlePaymentFailed(event: PaymentEvent): Promise<void>;
  abstract handlePaymentSucceeded(event: PaymentEvent): Promise<void>;
  abstract handlePaymentCreated(event: PaymentEvent): Promise<void>;
}

export type AccountingProcessorAndId = {
  processor: AccountingProcessor;
  processorId: string;
};
