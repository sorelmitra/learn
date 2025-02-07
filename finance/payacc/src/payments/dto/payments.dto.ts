import { IsEnum, IsNumber, IsObject, IsOptional, ValidateNested } from 'class-validator';
import { Customer, CustomerInput } from './customers.dto';
import { Type } from 'class-transformer';

export enum PaymentMethodComboInput {
  AchSuccess = 'AchSuccess',
  AchNotAuthorized = 'AchNotAuthorized',
}

export class CreatePaymentInput {
  @IsNumber()
  amount: number;

  @IsObject()
  @ValidateNested()
  @Type(() => CustomerInput)
  customer: CustomerInput;

  @IsEnum(PaymentMethodComboInput)
  @IsOptional()
  methodCombo?: PaymentMethodComboInput;
}

export class UpdatePaymentInput {
  @IsNumber()
  @IsOptional()
  amount?: number;

  @IsEnum(PaymentMethodComboInput)
  @IsOptional()
  methodCombo?: PaymentMethodComboInput;
}

export enum PaymentStatusName {
  Open = 'Open',
  Pending = 'Pending',
  Succeeded = 'Succeeded',
  Failed = 'Failed',
}

export enum PaymentMethodType {
  UsBankAccount = 'UsBankAccount',
}

export class PaymentMethod {
  type: PaymentMethodType;
  lastFour?: string;
  bankName?: string;
  routingNumber?: string;
}

export class Payment {
  id: string;
  amount: number;
  status: PaymentStatusName;
  customer?: Customer;
  method?: PaymentMethod;
}
