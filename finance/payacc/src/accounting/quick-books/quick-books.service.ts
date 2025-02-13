import { Injectable } from '@nestjs/common';
import { QueueConsumer } from 'src/common/queue/queue-consumer';
import { PaymentEvent } from 'src/payments/events/payments-events';

@Injectable()
export class QuickBooksService implements QueueConsumer {
  handle(event: PaymentEvent): Promise<void> {
    throw new Error('Method not implemented.');
  }
}
