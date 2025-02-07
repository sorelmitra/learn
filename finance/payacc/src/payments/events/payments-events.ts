import { Customer } from "../dto/customers.dto";
import { PaymentMethodType } from "../dto/payments.dto";

export enum PaymentEventName {}

export type PaymentEvent = {
	id: string;
	name: PaymentEventName;
	created: Date;
	customer: Customer;
	amount: number;
	method?: PaymentMethodType;
};
