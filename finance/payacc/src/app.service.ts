import { Injectable, Logger } from '@nestjs/common';
import { CreatePaymentInput, Payment, UpdatePaymentInput } from './payments/dto/payments.dto';
import { PaymentsProcessorService } from './payments/processor/payments-processor.service';
import { PaymentsProcessorName } from './payments/processor/payments-processor';

@Injectable()
export class AppService {
  constructor(
    private readonly logger: Logger,
    private readonly paymentsProcessorService: PaymentsProcessorService,
  ) {}

  getHello(): string {
    return 'Hello World!';
  }

  createPayment({
    input,
    proc,
  }: {
    input: CreatePaymentInput;
    proc: PaymentsProcessorName;
  }): Promise<Payment> {
    const processor = this.paymentsProcessorService.get(proc);
    return processor.createPayment(input);
  }

  async updatePayment({ id, input }: { id: string; input: UpdatePaymentInput }): Promise<Payment> {
    const { processor, processorId } = this.paymentsProcessorService.getFromPaymentId(id);
    return processor.updatePayment({ processorId, input });
  }

  async getPayment(id: string): Promise<Payment> {
    const { processor, processorId } = this.paymentsProcessorService.getFromPaymentId(id);
    return processor.getPayment(processorId);
  }

  async confirmPayment(id: string): Promise<Payment> {
    const { processor, processorId } = this.paymentsProcessorService.getFromPaymentId(id);
    return processor.confirmPayment(processorId);
  }
}
