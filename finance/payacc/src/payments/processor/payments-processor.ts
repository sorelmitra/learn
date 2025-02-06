import { CreatePaymentInput, Payment } from "../dto/payments.dto";

export interface PaymentsProcessor {
  createPayment(input: CreatePaymentInput): Promise<Payment>;
}
