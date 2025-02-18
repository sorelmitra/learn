import { Module } from '@nestjs/common';
import { CommonModule } from 'src/common/common.module';
import { AccountingProcessorFactory } from './accounting-processor/accounting-processor.factory';
import { QuickBooksService } from './quick-books/quick-books.service';

@Module({
  imports: [CommonModule],
  providers: [QuickBooksService, AccountingProcessorFactory],
  exports: [AccountingProcessorFactory],
})
export class AccountingModule {}
