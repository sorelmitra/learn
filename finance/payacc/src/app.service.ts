import { Injectable, Logger } from '@nestjs/common';
import { CreatePaymentInput, Payment, UpdatePaymentInput } from './payments/dto/payments.dto';
import { PaymentsProcessorFactory } from './payments/processor/payments-processor.factory';
import { PaymentsProcessorName } from './payments/processor/payments-processor';

@Injectable()
export class AppService {
  constructor(
    private readonly logger: Logger,
    private readonly paymentsProcessorFactory: PaymentsProcessorFactory,
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
    const processor = this.paymentsProcessorFactory.get(proc);
    return processor.createPayment(input);
  }

  async updatePayment({ id, input }: { id: string; input: UpdatePaymentInput }): Promise<Payment> {
    const { processor, processorId } = this.paymentsProcessorFactory.getFromPaymentId(id);
    return processor.updatePayment({ processorId, input });
  }

  async getPayment(id: string): Promise<Payment> {
    const { processor, processorId } = this.paymentsProcessorFactory.getFromPaymentId(id);
    return processor.getPayment(processorId);
  }

  async confirmPayment(id: string): Promise<Payment> {
    const { processor, processorId } = this.paymentsProcessorFactory.getFromPaymentId(id);
    return processor.confirmPayment(processorId);
  }
}
