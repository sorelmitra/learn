import { PaymentEvent } from "src/payments/events/payments-events";

export interface QueueConsumer {
	handle(event: PaymentEvent): Promise<void>;
}
