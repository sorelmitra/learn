import * as util from 'util';
import { Injectable, Logger } from '@nestjs/common';
import { StripeEvent } from 'src/payments/dto/stripe.dto';
import { QueueService } from 'src/common/queue/queue.service';

@Injectable()
export class StripeWebhookService {
  constructor(private readonly logger: Logger, private readonly queueService: QueueService) {}

  receive(event: StripeEvent) {
    this.logger.debug(
      `Received Stripe event ${event.type}, live mode ${event.livemode}`,
      util.inspect(event),
    );
    throw new Error('Method not implemented.');
  }
}
