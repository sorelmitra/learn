import OrderStatusChangeEvent from "../models/orderStatusChangeEvent";

export enum RepoObserverEvents {
	MODIFY = "MODIFY",
}

type EventHandler = (orderEvent: OrderStatusChangeEvent) => void;

export class RepoObserver {
	eventHandlers = {};
	eventKeyMapping = {
		id: ["NewImage", "id"],
		pizzaType: ["NewImage", "pizzaType"],
		oldStatus: ["OldImage", "status"],
		newStatus: ["NewImage", "status"],
	}
	constructor() {}

	on(event: RepoObserverEvents, handler: EventHandler) {
		this.eventHandlers[event] = handler;
	}

	process(dynamoDbRecords: any): number {
		let n = 0;
		dynamoDbRecords.forEach((record) => {
			console.log('DynamoDB Stream record', JSON.stringify(record, null, 2));

			if (record.eventName == RepoObserverEvents.MODIFY) {
				this.processModifyEvent(record);
				n++;
			}
		});
		return n;
	}

	private processModifyEvent(record: any) {
		let orderEvent: OrderStatusChangeEvent = new OrderStatusChangeEvent();
		for (let key in this.eventKeyMapping) {
			let image: string = this.eventKeyMapping[key][0];
			let imageKey: string = this.eventKeyMapping[key][1];
			if (undefined === record.dynamodb[image]) {
				orderEvent[key] = null;
			} else {
				orderEvent[key] = record.dynamodb[image][imageKey].S;
			}
		}
		console.log("orderEvent", orderEvent);
		this.invokeEventHandler(RepoObserverEvents.MODIFY, orderEvent);
	}

	private invokeEventHandler(event: RepoObserverEvents, orderEvent: OrderStatusChangeEvent) {
		if (this.eventHandlers[event]) {
			this.eventHandlers[event](orderEvent);
		}
	}

}
