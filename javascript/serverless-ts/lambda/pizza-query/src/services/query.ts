import { Db } from "../db/db";

class Query {
	db: Db;
	constructor(db: Db) {
		this.db = db;
	}

	run(request) {
		if (undefined === process.env.PIZZA_TYPES_TABLE) {
			return new Error("Missing environment variable PIZZA_TYPES_TABLE!");
		}
		return this.db.getAll({
			table: process.env.PIZZA_TYPES_TABLE
		});
	}
}

export default Query;
