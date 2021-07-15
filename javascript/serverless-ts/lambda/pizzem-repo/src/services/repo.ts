import { Db } from "../db/db";

class Repo {
	db: Db;
	constructor(db: Db) {
		this.db = db;
	}

	async getAllPizzas() {
		if (undefined === process.env.PIZZA_TYPES_TABLE) {
			return [{ error: "Missing environment variable PIZZA_TYPES_TABLE!" }];
		}
		return this.db.getAll({
			table: process.env.PIZZA_TYPES_TABLE
		});
	}

	async getPizzaByType(type: string) {
		if (undefined === process.env.PIZZA_TYPES_TABLE) {
			return [{ error: "Missing environment variable PIZZA_TYPES_TABLE!" }];
		}
		return this.db.getAll({
			table: process.env.PIZZA_TYPES_TABLE,
			filter: {
				name: "type",
				value: type
			}
		});
	}

	createPizza(pizza: Pizza) {
		if (undefined === process.env.PIZZA_TYPES_TABLE) {
			return [{ error: "Missing environment variable PIZZA_TYPES_TABLE!" }];
		}
		return this.db.create({
			table: process.env.PIZZA_TYPES_TABLE,
			data: pizza,
		});
	}

	createOrder(order: Order) {
		if (undefined === process.env.PIZZA_ORDERS_TABLE) {
			return [{ error: "Missing environment variable PIZZA_ORDERS_TABLE!" }];
		}
		return this.db.create({
			table: process.env.PIZZA_ORDERS_TABLE,
			data: order,
		});
	}

	patchOrder(id: string, payload: any) {
		if (undefined === process.env.PIZZA_ORDERS_TABLE) {
			return [{ error: "Missing environment variable PIZZA_ORDERS_TABLE!" }];
		}
		return this.db.patch({
			table: process.env.PIZZA_ORDERS_TABLE,
			patch: {
				id: {
					key: "id",
					value: id,
				},
				data: payload
			}
		});
	}

	deleteOrder(id: string) {
		if (undefined === process.env.PIZZA_ORDERS_TABLE) {
			return [{ error: "Missing environment variable PIZZA_ORDERS_TABLE!" }];
		}
		return this.db.delete({
			table: process.env.PIZZA_ORDERS_TABLE,
			id: id,
		});
	}
}

export default Repo;
