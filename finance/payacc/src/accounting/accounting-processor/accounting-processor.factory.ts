import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { QuickBooksService } from '../quick-books/quick-books.service';
import { AccountingProcessor, AccountingProcessorName } from './accounting-processor';
import { QueueConsumer } from 'src/common/queue/queue-consumer';
import { PaymentEvent } from 'src/payments/events/payments-events';
import { ConfigService } from '@nestjs/config';

@Injectable()
export class AccountingProcessorFactory implements QueueConsumer {
  constructor(
    private readonly configService: ConfigService,
    private readonly quickBooksService: QuickBooksService,
  ) {}

  private processorsMap = new Map<AccountingProcessorName, AccountingProcessor>([
    [AccountingProcessorName.QUICK_BOOKS, this.quickBooksService],
  ]);

  private get(proc: AccountingProcessorName): AccountingProcessor {
    const processor = this.processorsMap.get(proc ?? AccountingProcessorName.QUICK_BOOKS);
    if (!processor) {
      throw new HttpException(`Unknown processor ${proc}`, HttpStatus.BAD_REQUEST);
    }
    return processor;
  }

  async handle(event: PaymentEvent): Promise<void> {
    // TODO: this might come via a configuration endpoint so it is dynamic
    const processorName = this.configService.get<string>('ACCOUNTING_PROCESSOR_NAME');
    const processor = this.get(processorName as AccountingProcessorName);
    return processor.handle(event);
  }
}
