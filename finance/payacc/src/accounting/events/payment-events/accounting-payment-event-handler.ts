import { Injectable } from '@nestjs/common';
import { AccountingProcessorFactory } from 'src/accounting/accounting-processor/accounting-processor.factory';
import { PaymentEvent } from 'src/payments/events/payments-events';

@Injectable()
export abstract class AccountingPaymentEventHandler {
  constructor(private readonly accountingProcessorFactory: AccountingProcessorFactory) {}

  abstract handle(event: PaymentEvent): Promise<void>;

  getAccountingProcessor() {
    // TODO: add a configuration endpoint for the accounting processor name
    return this.accountingProcessorFactory.create();
  }
}

export class PaymentCreatedAccountingEventHandler extends AccountingPaymentEventHandler {
  async handle(event: PaymentEvent): Promise<void> {
    const accountingProcessor = this.getAccountingProcessor();
    return accountingProcessor.handlePaymentCreated(event);
  }
}

export class PaymentFailedAccountingEventHandler extends AccountingPaymentEventHandler {
  async handle(event: PaymentEvent): Promise<void> {
    const accountingProcessor = this.getAccountingProcessor();
    return accountingProcessor.handlePaymentFailed(event);
  }
}

export class PaymentSucceededAccountingEventHandler extends AccountingPaymentEventHandler {
  async handle(event: PaymentEvent): Promise<void> {
    const accountingProcessor = this.getAccountingProcessor();
    return accountingProcessor.handlePaymentSucceeded(event);
  }
}
