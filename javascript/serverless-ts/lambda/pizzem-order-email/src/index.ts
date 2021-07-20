import SES from "aws-sdk/clients/ses";
import {OrderEmailSend} from "./services/orderEmailSend";

export const handler = async (event, context) => {
    console.log("Event records", event.Records);

    try {
        let orderEmailQueue: OrderEmailSend = new OrderEmailSend(new SES(), process.env.ORDER_EMAIL_RECIPIENT_INFO);
        await orderEmailQueue.process(event.Records);
    } catch (error) {
        console.error(error);
    }
};
