import { Injectable, Logger } from '@nestjs/common';
import { QueueConsumer } from 'src/common/queue/queue-consumer';
import { PaymentEvent, PaymentEventName } from 'src/payments/events/payments-events';
import {
  AccountingPaymentEventHandler,
  PaymentCreatedAccountingEventHandler,
  PaymentFailedAccountingEventHandler,
  PaymentSucceededAccountingEventHandler,
} from './accounting-payment-event-handler';

@Injectable()
export class PaymentAccountingEventHandlerFactory implements QueueConsumer {
  constructor(
    private readonly logger: Logger,
    private readonly paymentCreatedAccountingEventHandler: PaymentCreatedAccountingEventHandler,
    private readonly paymentFailedAccountingEventHandler: PaymentFailedAccountingEventHandler,
    private readonly paymentSucceededAccountingEventHandler: PaymentSucceededAccountingEventHandler,
  ) {}

  private eventHandlersMapping = new Map<PaymentEventName, AccountingPaymentEventHandler>([
    [PaymentEventName.PaymentCreated, this.paymentCreatedAccountingEventHandler],
    [PaymentEventName.PaymentFailed, this.paymentFailedAccountingEventHandler],
    [PaymentEventName.PaymentSucceeded, this.paymentSucceededAccountingEventHandler],
  ]);

  async handle(event: PaymentEvent): Promise<void> {
    const handler = this.eventHandlersMapping.get(event.name);
    if (!handler) {
      this.logger.debug(`Ignoring Payment event ${event.name}, as no handler was found for it`);
      return;
    }
    return handler.handle(event);
  }
}
