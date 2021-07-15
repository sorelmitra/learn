import { Db } from "../db/db";

class Repo {
	db: Db;
	constructor(db: Db) {
		this.db = db;
	}

	async getAll() {
		if (undefined === process.env.PIZZA_TYPES_TABLE) {
			return [{ error: "Missing environment variable PIZZA_TYPES_TABLE!" }];
		}
		return this.db.getAll({
			table: process.env.PIZZA_TYPES_TABLE
		});
	}

	async getByType(type: string) {
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

	create(pizza: Pizza) {
		if (undefined === process.env.PIZZA_TYPES_TABLE) {
			return [{ error: "Missing environment variable PIZZA_TYPES_TABLE!" }];
		}
		return this.db.create({
			table: process.env.PIZZA_TYPES_TABLE,
			data: pizza,
		});
	}
}

export default Repo;
