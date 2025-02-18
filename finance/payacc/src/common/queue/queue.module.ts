import { Logger, Module } from '@nestjs/common';
import { AccountingModule } from 'src/accounting/accounting.module';
import { PaymentAccountingEventHandlerFactory } from 'src/accounting/events/payment-events/accounting-payment-event-handler.factory';
import { CommonModule } from '../common.module';
import { QueueService } from './queue.service';

@Module({
  imports: [CommonModule, AccountingModule],
  providers: [
    {
      provide: QueueService,
      inject: [Logger, PaymentAccountingEventHandlerFactory],
      useFactory: (
        logger: Logger,
        accountingEventHandlerFactory: PaymentAccountingEventHandlerFactory,
      ) => {
        const queueService = new QueueService(logger);
        queueService.register(accountingEventHandlerFactory);
        return queueService;
      },
    },
  ],
  exports: [QueueService],
})
export class QueueModule {}
