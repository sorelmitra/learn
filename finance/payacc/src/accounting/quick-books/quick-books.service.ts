import { Injectable, Logger } from '@nestjs/common';
import { PaymentEvent } from 'src/payments/events/payments-events';
import { AccountingProcessor } from '../accounting-processor/accounting-processor';
import { IntuitOAuthService } from '../intuit/intuit-oauth/intuit-oauth.service';

@Injectable()
export class QuickBooksService extends AccountingProcessor {
  constructor(
    private readonly logger: Logger,
    private readonly intuitOAuthService: IntuitOAuthService,
  ) {
    super();
  }

  async handlePaymentCreated(event: PaymentEvent): Promise<void> {
    this.logger.debug(`Ignoring event ${event.name} for accounting purposes`);
  }

  async handlePaymentSucceeded(event: PaymentEvent): Promise<void> {
    this.logger.debug(`Handing accounting for event ${event.name}`);
    await this.intuitOAuthService.makeApiCall('/payment', 'POST', {});
  }

  async handlePaymentFailed(event: PaymentEvent): Promise<void> {
    this.logger.debug(`Ignoring event ${event.name} for accounting purposes`);
  }
}
