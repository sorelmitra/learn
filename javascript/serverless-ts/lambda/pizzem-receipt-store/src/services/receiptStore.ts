import S3 from "aws-sdk/clients/s3";
import {ReceiptStoreMessage} from "../models/receiptStoreMessage";
import {OrderReceipt} from "../models/orderReceipt";

export class ReceiptStore {
    private s3: S3;
    private readonly bucket: string | undefined;

    constructor(s3: S3, bucket: string | undefined) {
        this.s3 = s3;
        this.bucket = bucket;
    }

    async process(sqsRecords: any[]) {
        for (const record of sqsRecords) {
            let storeMessage: ReceiptStoreMessage = JSON.parse(record.body);
            console.log("Storing receipt", storeMessage);
            let key: string = this.computeKey(storeMessage);
            let receipt: OrderReceipt = {
                pizzaType: storeMessage.pizzaType,
                amount: storeMessage.amount,
                date: storeMessage.date
            }
            return this.upload(key, receipt);
        }
    }

    private computeKey(storeMessage: ReceiptStoreMessage) {
        let date: Date = new Date(storeMessage.date);
        let yyyy = new Intl.DateTimeFormat('en', {year: "numeric"}).format(date);
        let mm = new Intl.DateTimeFormat('en', {month: "2-digit"}).format(date);
        let dd = new Intl.DateTimeFormat('en', {day: "2-digit"}).format(date);
        return `${yyyy}-${mm}-${dd}_${storeMessage.id}_${storeMessage.pizzaType}.json`;
    }

    private upload(key: string, receipt: OrderReceipt) {
        if (undefined === this.bucket) {
            throw "Missing bucket name!";
        }

        let uploadParams = {
            Bucket: this.bucket,
            Key: key,
            Body: JSON.stringify(receipt)
        };

        return this.s3.upload(uploadParams).promise();
    }
}