import SES from "aws-sdk/clients/ses";
import {OrderEmailRequest} from "../models/orderEmailRequest";
import {EmailRecipient} from "../models/emailRecipient";

export class OrderEmailSend {
    private ses: SES;
    private recipient: EmailRecipient;

    constructor(ses: SES, recipientInfo: string | undefined) {
        this.ses = ses;
        this.recipient = this.parseRecipient(recipientInfo);
    }

    private async process(sqsRecords: any) {
        for (let record of sqsRecords) {
            let emailRequest: OrderEmailRequest = JSON.parse(record.body);
            let [subject, body] = this.computeSubjectAndBody(emailRequest);
            console.log(`Sending email to ${this.recipient.email}, subject '${subject}', body\n`, body);
            return this.sendEmail(subject, body);
        }
    }

    private parseRecipient(recipientInfo: string | undefined): EmailRecipient {
        if (undefined === recipientInfo) {
            throw "Missing recipient info!";
        }
        let emailRecipient: EmailRecipient = JSON.parse(recipientInfo);
        return emailRecipient;
    }

    private computeSubjectAndBody(emailRequest: OrderEmailRequest) {
        let subject: string = `Order confirmation #879394733`;
        let body: string = `Greetings ${this.recipient.firstName}!\n\nYour ${emailRequest.pizzaType} pizza order has changed status from '${emailRequest.oldStatus}' to '${emailRequest.newStatus}'!\n\nWith respect,\nThe Pizzem Team`;
        return [subject, body];
    }

    private async sendEmail(subject: string, body: string) {
        let params = {
            Destination: {
                ToAddresses: [this.recipient.email]
            },
            Message: {
                Body: {
                    Text: {
                        Charset: "UTF-8",
                        Data: body
                    }
                },
                Subject: {
                    Charset: 'UTF-8',
                    Data: subject
                }
            },
            Source: this.recipient.email,
        };

        return this.ses.sendEmail(params).promise();
    }
}