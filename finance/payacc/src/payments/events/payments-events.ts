import { Customer } from '../dto/customers.dto';
import { PaymentMethod } from '../dto/payments.dto';

export enum PaymentEventName {
  PaymentCreated = 'PaymentCreated',
  PaymentFailed = 'PaymentFailed',
  PaymentSucceeded = 'PaymentSucceeded',
}

export type PaymentEvent = {
  id: string;
  name: PaymentEventName;
  created: Date;
  amount: number;
  customer?: Customer;
  method?: PaymentMethod;
};
