import { IsEnum, IsNumber, IsOptional } from 'class-validator';
import { PaymentMethodName } from '../processor/payments-processor';

export class CreatePaymentInput {
  @IsNumber()
  amount: number;

  @IsEnum(PaymentMethodName)
  @IsOptional()
  method?: PaymentMethodName;
}

export enum PaymentStatusName {
  Open = 'Open',
  Pending = 'Pending',
  Succeeded = 'Succeeded',
  Failed = 'Failed',
}

export class Payment {
  id: string;
  status: PaymentStatusName;
}
