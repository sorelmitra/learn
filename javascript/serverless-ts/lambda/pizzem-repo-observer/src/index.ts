import SNS from 'aws-sdk/clients/sns';

import {RepoObserver, RepoObserverEvents} from './services/repoObserver';
import { EventNotifierPublishResult, RepoEventsNotifier } from './services/repoEventsNotifier';
import OrderStatusChangeEvent from './models/orderStatusChangeEvent';

export const handler = async (event, context) => {

	let repoObserver: RepoObserver = new RepoObserver();
	let repoEventsNotifier: RepoEventsNotifier = new RepoEventsNotifier(process.env.ORDER_EVENTS_SNS_TOPIC_ARN, new SNS());

	return new Promise<EventNotifierPublishResult[]>((resolve, reject) => {
		let publishedEvents: EventNotifierPublishResult[] = [];
		repoObserver.on(RepoObserverEvents.INSERT, 
			async (orderEvent: OrderStatusChangeEvent) => {
				try {
					let r: EventNotifierPublishResult = await repoEventsNotifier.publish(orderEvent);
					publishedEvents.push(r);
					if (publishedEvents.length == count) {
						resolve(publishedEvents);
					}
				} catch (error) {
					reject(error);
				}
			});
		
		let count: number = repoObserver.process(event.Records);
	});
};
