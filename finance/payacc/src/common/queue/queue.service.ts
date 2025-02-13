import { Injectable, Logger } from '@nestjs/common';
import { PaymentEvent } from 'src/payments/events/payments-events';
import { QueueConsumer } from './queue-consumer';
import { Interval } from '@nestjs/schedule';

@Injectable()
export class QueueService {
  constructor(private readonly logger: Logger) {}

  private queue: PaymentEvent[] = [];
  private consumers: QueueConsumer[] = [];

  register(consumer: QueueConsumer) {
    this.consumers.push(consumer);
  }

  add(event: PaymentEvent) {
    this.logger.debug(`Queuing payment event ${event.name} / ${event.id}`);
    this.queue.push(event);
  }

  @Interval(100)
  async fireOnce() {
    const event = this.queue.shift();
    if (!event) return;
    this.logger.debug(`Sending payment event ${event.name} / ${event.id} to consumers`);
    for (const consumer of this.consumers) {
      await consumer.handle(event);
    }
  }
}
