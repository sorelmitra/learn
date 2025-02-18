import { Module } from '@nestjs/common';
import { ScheduleModule } from '@nestjs/schedule';
import { AccountingModule } from './accounting/accounting.module';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { CommonModule } from './common/common.module';
import { QueueModule } from './common/queue/queue.module';
import { PaymentsProcessorFactory } from './payments/processor/payments-processor.factory';
import { StripeModule } from './payments/stripe/stripe.module';

@Module({
  imports: [ScheduleModule.forRoot(), StripeModule, CommonModule, QueueModule, AccountingModule],
  controllers: [AppController],
  providers: [AppService, PaymentsProcessorFactory],
})
export class AppModule {}
