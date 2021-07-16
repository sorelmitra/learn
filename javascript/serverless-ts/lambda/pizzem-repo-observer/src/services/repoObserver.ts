import OrderStatusChangeEvent from "../models/orderStatusChangeEvent";

export enum RepoObserverEvents {
	INSERT = "INSERT",
}

type EventHandler = (orderEvent: OrderStatusChangeEvent) => void;

export class RepoObserver {
	eventHandlers = {};
	constructor() {}

	on(event: RepoObserverEvents, handler: EventHandler) {
		this.eventHandlers[event] = handler;
	}

	process(dynamoDbRecords: any) {
		dynamoDbRecords.forEach((record) => {
			console.log('DynamoDB Stream record', JSON.stringify(record, null, 2));

			if (record.eventName == 'INSERT') {
				this.processInsertEvent(record);
			}
		});
	}

	private processInsertEvent(record: any) {
		let orderEvent: OrderStatusChangeEvent = new OrderStatusChangeEvent();
		let keyMapping = {
			id: ["NewImage", "id"],
			pizzaType: ["NewImage", "pizzaType"],
			oldStatus: ["Keys", "status"],
			newStatus: ["NewImage", "status"],
		}
		for (let key in keyMapping) {
			let image: string = keyMapping[key][0];
			let imageKey: string = keyMapping[key][1];
			orderEvent[key] = record.dynamodb[image][imageKey].S;
		}
		console.log("orderEvent", orderEvent);
		this.invokeEventHandler(RepoObserverEvents.INSERT, orderEvent);
	}

	private invokeEventHandler(event: RepoObserverEvents, orderEvent: OrderStatusChangeEvent) {
		if (this.eventHandlers[event]) {
			this.eventHandlers[event](orderEvent);
		}
	}

}
