import SQS from "aws-sdk/clients/sqs";
import {ReceiptStoreQueue} from "./services/receiptStoreQueue";

export const handler = async (event, context) => {
    console.log("Event records", event.Records);

    let receiptStoreQueue: ReceiptStoreQueue = new ReceiptStoreQueue(new SQS(), process.env.RECEIPT_STORE_QUEUE_URL);
    try {
        await receiptStoreQueue.process(event.Records);
    } catch (error) {
        console.log(error);
    }
};
