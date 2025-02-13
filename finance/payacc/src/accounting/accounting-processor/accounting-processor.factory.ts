import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { QuickBooksService } from '../quick-books/quick-books.service';
import { AccountingProcessor, AccountingProcessorAndId, AccountingProcessorName } from './accounting-processor';

@Injectable()
export class AccountingProcessorFactory {
  constructor(private quickBooksService: QuickBooksService) {}

  private processorsMap = new Map<AccountingProcessorName, AccountingProcessor>([
    [AccountingProcessorName.QUICK_BOOKS, this.quickBooksService],
  ]);

  get(proc: AccountingProcessorName): AccountingProcessor {
    const processor = this.processorsMap.get(proc ?? AccountingProcessorName.QUICK_BOOKS);
    if (!processor) {
      throw new HttpException(`Unknown processor ${proc}`, HttpStatus.BAD_REQUEST);
    }
    return processor;
  }
}
