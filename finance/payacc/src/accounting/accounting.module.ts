import { Module } from '@nestjs/common';
import { CommonModule } from 'src/common/common.module';
import { AccountingProcessorFactory } from './accounting-processor/accounting-processor.factory';
import { QuickBooksService } from './quick-books/quick-books.service';
import { PaymentAccountingEventHandlerFactory } from './events/payment-events/accounting-payment-event-handler.factory';
import {
  PaymentCreatedAccountingEventHandler,
  PaymentFailedAccountingEventHandler,
  PaymentSucceededAccountingEventHandler,
} from './events/payment-events/accounting-payment-event-handler';

@Module({
  imports: [CommonModule],
  providers: [
    QuickBooksService,
    AccountingProcessorFactory,
    PaymentCreatedAccountingEventHandler,
    PaymentFailedAccountingEventHandler,
    PaymentSucceededAccountingEventHandler,
    PaymentAccountingEventHandlerFactory,
  ],
  exports: [AccountingProcessorFactory, PaymentAccountingEventHandlerFactory],
})
export class AccountingModule {}
