import { Injectable, Logger } from '@nestjs/common';
import { PaymentEvent } from 'src/payments/events/payments-events';
import { QueueConsumer } from './queue-consumer';

@Injectable()
export class QueueService {
  constructor(private readonly logger: Logger) { }

  private events: PaymentEvent[];
  private consumers: QueueConsumer[];

  register(consumer: QueueConsumer) {
    this.consumers.push(consumer);
  }

  add(event: PaymentEvent) {
    this.logger.debug(`Queuing payment event`, event);
    this.events.push(event);
  }
}
