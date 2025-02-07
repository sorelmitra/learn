import { IsEnum, IsNumber, IsObject, IsOptional, ValidateNested } from 'class-validator';
import { PaymentMethodName } from '../processor/payments-processor';
import { Customer, CustomerInput } from './customers.dto';
import { Type } from 'class-transformer';

export class CreatePaymentInput {
  @IsNumber()
  amount: number;
  
  @IsObject()
  @ValidateNested()
  @Type(() => CustomerInput)
  customer: CustomerInput;

  @IsEnum(PaymentMethodName)
  @IsOptional()
  method?: PaymentMethodName;
}

export class UpdatePaymentInput {
  @IsNumber()
  @IsOptional()
  amount?: number;

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
  amount: number;
  status: PaymentStatusName;
  customer?: Customer;
}
