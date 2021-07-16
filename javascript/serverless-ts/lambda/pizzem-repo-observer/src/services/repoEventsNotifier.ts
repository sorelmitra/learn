import SNS from 'aws-sdk/clients/sns';

import OrderStatusChangeEvent from "../models/orderStatusChangeEvent";

export class EventNotifierPublishResult {
	originalEvent: OrderStatusChangeEvent = new OrderStatusChangeEvent();
	status: string = "";
};

export class RepoEventsNotifier {
	topicArn: string | undefined;
	sns: SNS;

	constructor(topicArn: string | undefined, sns: SNS) {
		this.topicArn = topicArn;
		this.sns = sns;
	}

	async publish(orderEvent: OrderStatusChangeEvent): Promise<EventNotifierPublishResult> {
		let params: SNS.PublishInput = {
			Subject: `Pizza order ${orderEvent.id} changed from ${orderEvent.oldStatus} to ${orderEvent.newStatus}`,
			Message: JSON.stringify(orderEvent),
			TopicArn: this.topicArn,
		};
		let result: EventNotifierPublishResult = new EventNotifierPublishResult();
		result.originalEvent = orderEvent;
		try {
			let r = await this.sns.publish(params).promise();
			result.status = `Success: Published to ${this.topicArn}`;
			console.log(`Event ${orderEvent} - ${result.status}, response`, r);
		} catch(error) {
			console.log(error);
			result.status = error;
		}
		return result;
	}

}
