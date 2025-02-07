import { Logger, Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { ConfigModule, ConfigService } from '@nestjs/config';
import { PaymentsProcessorService } from './payments/processor/payments-processor.service';
import { StripeService } from './payments/stripe/stripe.service';
import { StripeWebhookService } from './payments/stripe/webhook/stripe-webhook.service';

@Module({
  imports: [
    ConfigModule.forRoot({
      envFilePath: `.${process.env.ENV}.env`,
    }),
  ],
  controllers: [AppController],
  providers: [AppService, Logger, ConfigService, PaymentsProcessorService, StripeService, StripeWebhookService],
})
export class AppModule {}
