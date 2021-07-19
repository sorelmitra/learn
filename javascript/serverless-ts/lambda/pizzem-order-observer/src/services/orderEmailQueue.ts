import SQS from "aws-sdk/clients/sqs";
import OrderStatusChangeEvent from "../models/orderStatusChangeEvent";
import {OrderEmailMessage} from "../models/orderEmailMessage";

export class OrderEmailQueue {
    private sqs: SQS;
    private queueUrl: string | undefined;

    constructor(sqs: SQS, queueUrl: string | undefined) {
        this.sqs = sqs;
        this.queueUrl = queueUrl;
    }

    async process(snsRecords: any[]) {
        for (const record of snsRecords) {
            let orderEvent: OrderStatusChangeEvent = JSON.parse(record.Sns.Message);
            await this.enqueue(orderEvent);
            console.log("Order event enqueued", orderEvent);
        }
    }

    private enqueue(orderEvent: OrderStatusChangeEvent) {
        if (undefined === this.queueUrl) {
            throw "Undefined queue URL!";
        }
        let params: SQS.SendMessageRequest = {
            QueueUrl: this.queueUrl,
            MessageBody: this.computeQueueMessage(orderEvent),
        };
        return this.sqs.sendMessage(params).promise();
    }

    private computeQueueMessage(orderEvent: OrderStatusChangeEvent) {
        let message: OrderEmailMessage = new OrderEmailMessage();
        message.pizzaType = orderEvent.pizzaType;
        message.newStatus = orderEvent.newStatus;
        message.oldStatus = orderEvent.oldStatus;
        console.log("Message to enqueue", message);
        return JSON.stringify(message);
    }
}