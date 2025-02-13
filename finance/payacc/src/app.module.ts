import { Logger, Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { ConfigModule, ConfigService } from '@nestjs/config';
import { PaymentsProcessorFactory } from './payments/processor/payments-processor.factory';
import { StripeService } from './payments/stripe/stripe.service';
import { StripeWebhookService } from './payments/stripe/webhook/stripe-webhook.service';
import { QueueService } from './common/queue/queue.service';
import { StripeEventHandlerFactory } from './payments/stripe/events/stripe-event-handler.factory';
import { StripeEventHandlerPaymentIntentCreated } from './payments/stripe/events/stripe-event-handler-payment-intent-created';
import { StripeEventHandlerPaymentIntentFailed } from './payments/stripe/events/stripe-event-handler-payment-intent-failed';
import { StripeEventHandlerPaymentIntentSucceeded } from './payments/stripe/events/stripe-event-handler-payment-intent-succeeded';
import { AccountingProcessorFactory } from './accounting/accounting-processor/accounting-processor.factory';
import { QuickBooksService } from './accounting/quick-books/quick-books.service';
import { ScheduleModule } from '@nestjs/schedule';

@Module({
  imports: [
    ConfigModule.forRoot({
      envFilePath: `.${process.env.ENV}.env`,
    }),
    ScheduleModule.forRoot(),
  ],
  controllers: [AppController],
  providers: [
    AppService,
    Logger,
    ConfigService,
    {
      provide: QueueService,
      inject: [Logger, QuickBooksService],
      useFactory: (logger: Logger, quickBooksService: QuickBooksService) => {
        const queueService = new QueueService(logger);
        queueService.register(quickBooksService);
        return queueService;
      },
    },
    PaymentsProcessorFactory,
    StripeService,
    StripeWebhookService,
    StripeEventHandlerFactory,
    StripeEventHandlerPaymentIntentCreated,
    StripeEventHandlerPaymentIntentFailed,
    StripeEventHandlerPaymentIntentSucceeded,
    AccountingProcessorFactory,
    QuickBooksService,
  ],
})
export class AppModule {}
