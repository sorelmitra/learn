import { Logger, Module } from '@nestjs/common';
import { QueueService } from './queue.service';
import { CommonModule } from '../common.module';
import { AccountingProcessorFactory } from 'src/accounting/accounting-processor/accounting-processor.factory';
import { AccountingModule } from 'src/accounting/accounting.module';

@Module({
  imports: [CommonModule, AccountingModule],
  providers: [
    {
      provide: QueueService,
      inject: [Logger, AccountingProcessorFactory],
      useFactory: (logger: Logger, accountingProcessorFactory: AccountingProcessorFactory) => {
        const queueService = new QueueService(logger);
        queueService.register(accountingProcessorFactory);
        return queueService;
      },
    },
  ],
  exports: [QueueService],
})
export class QueueModule {}
