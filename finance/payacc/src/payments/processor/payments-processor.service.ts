import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { StripeService } from '../stripe/stripe.service';
import { PaymentsProcessor } from './payments-processor';

export enum PaymentsProcessorName {
  STRIPE = 'stripe',
}

@Injectable()
export class PaymentsProcessorService {
  constructor(private stripeService: StripeService) {}

  private processorsMap = new Map<PaymentsProcessorName, PaymentsProcessor>([
    [PaymentsProcessorName.STRIPE, this.stripeService],
  ]);

  get(proc: PaymentsProcessorName): PaymentsProcessor {
    const processor = this.processorsMap.get(
      proc ?? PaymentsProcessorName.STRIPE,
    );
    if (!processor) {
      throw new HttpException(
        `Unknown processor ${proc}`,
        HttpStatus.BAD_REQUEST,
      );
    }
    return processor;
  }
}
