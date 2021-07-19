import SQS from "aws-sdk/clients/sqs";
import {OrderEmailQueue} from "./services/orderEmailQueue";
import {ReceiptStoreQueue} from "../../pizzem-receipt-observer/src/services/receiptStoreQueue";

export const handler = async (event, context) => {
    console.log("Event records", event.Records);

    let orderEmailQueue: OrderEmailQueue = new OrderEmailQueue(new SQS(), process.env.ORDER_EMAIL_QUEUE_URL);
    try {
        await orderEmailQueue.process(event.Records);
    } catch (error) {
        console.log(error);
    }
};
