import {ReceiptStore} from "./services/receiptStore";
import S3 from "aws-sdk/clients/s3";

export const handler = async (event, context) => {
    console.log("Event records", event.Records);

    let store: ReceiptStore = new ReceiptStore(new S3(), process.env.RECEIPT_STORE_BUCKET);
    try {
        await store.process(event.Records);
    } catch (error) {
        console.log(error);
    }
};
