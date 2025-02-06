import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { StripeService } from '../stripe/stripe.service';
import {
  PaymentsProcessor,
  PaymentsProcessorAndId,
  PaymentsProcessorName,
} from './payments-processor';

@Injectable()
export class PaymentsProcessorService {
  constructor(private stripeService: StripeService) {}

  private processorsMap = new Map<PaymentsProcessorName, PaymentsProcessor>([
    [PaymentsProcessorName.STRIPE, this.stripeService],
  ]);

  get(proc: PaymentsProcessorName): PaymentsProcessor {
    const processor = this.processorsMap.get(proc ?? PaymentsProcessorName.STRIPE);
    if (!processor) {
      throw new HttpException(`Unknown processor ${proc}`, HttpStatus.BAD_REQUEST);
    }
    return processor;
  }

  getFromPaymentId(id: string): PaymentsProcessorAndId {
    const matches = id.match(/^([^_]+)_(.*)$/);
    const proc = matches?.at(1);
    const processorId = matches?.at(2);
    if (!proc || !processorId) {
      throw new HttpException(
        `Payment ID ${id} does not match the '<processor-name>_<processor-id>' format`,
        HttpStatus.BAD_REQUEST,
      );
    }
    return {
      processor: this.get(proc as PaymentsProcessorName),
      processorId,
    };
  }
}
