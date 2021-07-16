import SNS from 'aws-sdk/clients/sns';

import orderStatusChangeEvent from "../models/orderStatusChangeEvent";

class RepoEventsNotifier {
	topicArn: string | undefined;
	sns: SNS;

	constructor(topicArn: string | undefined, sns: SNS) {
		this.topicArn = topicArn;
		this.sns = sns;
	}

	async publish(orderEvent: orderStatusChangeEvent) {
		let params: SNS.PublishInput = {
			Subject: `Pizza order ${orderEvent.id} changed from ${orderEvent.oldStatus} to ${orderEvent.newStatus}`,
			Message: JSON.stringify(orderEvent),
			MessageStructure: "json",
			TopicArn: this.topicArn,
		};
		try {
			let r = this.sns.publish(params);
			console.log(`Published ${orderEvent} to ${this.topicArn}, response`, r);
		} catch(error) {
			console.log(error);
		}
	}

}

export default RepoEventsNotifier;
