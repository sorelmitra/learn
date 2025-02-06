import { IsEnum, IsNumber, IsOptional } from 'class-validator';
import { PaymentMethodName } from '../processor/payments-processor';

export class CreatePaymentInput {
  @IsNumber()
  amount: number;

  @IsEnum(PaymentMethodName)
  @IsOptional()
  paymentMethod?: PaymentMethodName;
}

export class Payment {
  id: string;
}
