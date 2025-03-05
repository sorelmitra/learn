import { Module } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import * as OAuthClient from 'intuit-oauth';
import { CommonModule } from 'src/common/common.module';
import { AccountingConnectionFactory } from './accounting-connection/accounting-connection.factory';
import { AccountingProcessorFactory } from './accounting-processor/accounting-processor.factory';
import {
  PaymentCreatedAccountingEventHandler,
  PaymentFailedAccountingEventHandler,
  PaymentSucceededAccountingEventHandler,
} from './events/payment-events/accounting-payment-event-handler';
import { PaymentAccountingEventHandlerFactory } from './events/payment-events/accounting-payment-event-handler.factory';
import { IntuitConnectionService } from './intuit/intuit-connection/intuit-connection.service';
import { QuickBooksService } from './quick-books/quick-books.service';
import { IntuitOauthService } from './intuit/intuit-oauth/intuit-oauth.service';

@Module({
  imports: [CommonModule],
  providers: [
    QuickBooksService,
    AccountingProcessorFactory,
    PaymentCreatedAccountingEventHandler,
    PaymentFailedAccountingEventHandler,
    PaymentSucceededAccountingEventHandler,
    PaymentAccountingEventHandlerFactory,
    {
      provide: OAuthClient,
      inject: [ConfigService],
      useFactory: (configService: ConfigService) =>
        new OAuthClient({
          clientId: configService.get('QB_CLIENT_ID'),
          clientSecret: configService.get('QB_CLIENT_SECRET'),
          environment: configService.get('QB_ENVIRONMENT'),
          redirectUri: configService.get('QB_REDIRECT_URI'),
        }),
    },
    IntuitConnectionService,
    AccountingConnectionFactory,
    IntuitOauthService,
  ],
  exports: [
    AccountingProcessorFactory,
    PaymentAccountingEventHandlerFactory,
    IntuitConnectionService,
    AccountingConnectionFactory,
    IntuitOauthService,
  ],
})
export class AccountingModule {}
