import { PaymentEvent } from 'src/payments/events/payments-events';

export enum AccountingProcessorName {
  QUICK_BOOKS = 'QuickBooks',
}

export abstract class AccountingProcessor {
  abstract handle(event: PaymentEvent): Promise<void>;
}

export type AccountingProcessorAndId = {
  processor: AccountingProcessor;
  processorId: string;
};
