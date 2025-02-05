import { IsNumber } from 'class-validator';

export class CreatePaymentInput {
  @IsNumber()
  amount: number;
}

export class Payment {
  id: string;
}
