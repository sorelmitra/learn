import SNS from 'aws-sdk/clients/sns';

import {RepoObserver, RepoObserverEvents} from './services/repoObserver';
import RepoEventsNotifier from './services/repoEventsNotifier';
import OrderStatusChangeEvent from './models/orderStatusChangeEvent';

export const handler = async (event, context) => {

	let repoObserver: RepoObserver = new RepoObserver();
	let repoEventsNotifier: RepoEventsNotifier = new RepoEventsNotifier(process.env.ORDER_EVENTS_SNS_TOPIC_ARN, new SNS());

	repoObserver.on(RepoObserverEvents.INSERT, 
		async (orderEvent: OrderStatusChangeEvent) => {
			await repoEventsNotifier.publish(orderEvent);
		});
	
	repoObserver.process(event.Records);
	return `Processed ${0} events`;
};
