import { Db, DbModel, DbOptions } from "./db";

class DbDynamo implements Db {
	getAll(options: DbOptions): DbModel[] {
		let values: DbModel[] = [];
		values.push({type: "not", description: "implemented"});
		return values;
	}
}

export default DbDynamo;
