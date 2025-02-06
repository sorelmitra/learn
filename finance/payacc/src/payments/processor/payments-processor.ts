import { CreatePaymentInput, Payment } from '../dto/payments.dto';

export enum PaymentsProcessorName {
  STRIPE = 'stripe',
}

export enum PaymentMethodName {
  Success = 'Success',
  AchNotAuthorized = 'AchNotAuthorized',
}

export interface PaymentsProcessor {
  createPayment(input: CreatePaymentInput): Promise<Payment>;
  confirmPayment(processorId: string): Promise<Payment>;
}

export type PaymentsProcessorAndId = {
  processor: PaymentsProcessor;
  processorId: string;
};

export const makeId = (proc: PaymentsProcessorName) => (processorId: string) =>
  `${proc}_${processorId}`;
