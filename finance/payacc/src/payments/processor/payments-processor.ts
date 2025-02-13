import { CreatePaymentInput, Payment, UpdatePaymentInput } from '../dto/payments.dto';

export enum PaymentsProcessorName {
  STRIPE = 'stripe',
}

export interface PaymentsProcessor {
  getPayment(processorId: string): Promise<Payment>;
  createPayment(input: CreatePaymentInput): Promise<Payment>;
  updatePayment({
    processorId,
    input,
  }: {
    processorId: string;
    input: UpdatePaymentInput;
  }): Promise<Payment>;
  confirmPayment(processorId: string): Promise<Payment>;
}

export type PaymentsProcessorAndId = {
  processor: PaymentsProcessor;
  processorId: string;
};
