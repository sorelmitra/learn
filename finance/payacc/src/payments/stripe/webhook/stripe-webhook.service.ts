import * as util from 'util';
import { Injectable, Logger } from '@nestjs/common';
import { StripeEvent } from 'src/payments/dto/stripe.dto';
import { QueueService } from 'src/common/queue/queue.service';
import { StripeEventHandlerFactory } from '../events/stripe-event-handler-factory';

@Injectable()
export class StripeWebhookService {
  constructor(
    private readonly logger: Logger,
    private readonly stripeEventHandlerFactory: StripeEventHandlerFactory,
    private readonly queueService: QueueService,
  ) {}

  async receive(stripeEvent: StripeEvent) {
    this.logger.debug(
      `Received Stripe event ${stripeEvent.type}, live mode ${stripeEvent.livemode}`,
      util.inspect(stripeEvent),
    );
    const handler = this.stripeEventHandlerFactory.create(stripeEvent.type);
    if (!handler) return;
    const event = await handler.handle(stripeEvent);
    this.queueService.add(event);
  }
}
