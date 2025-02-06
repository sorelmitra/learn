import { Injectable, Logger } from '@nestjs/common';
import { CreatePaymentInput, Payment } from './payments/dto/payments.dto';
import { PaymentsProcessorName, PaymentsProcessorService } from './payments/processor/payments-processor.service';

@Injectable()
export class AppService {
  constructor(
    private readonly logger: Logger,
    private readonly paymentsProcessorService: PaymentsProcessorService,
  ) {}

  createPayment({ input, proc }: { input: CreatePaymentInput, proc: PaymentsProcessorName }): Promise<Payment> {
    const processor = this.paymentsProcessorService.get(proc);
    return processor.createPayment(input);
  }

  getHello(): string {
    return 'Hello World!';
  }
}
