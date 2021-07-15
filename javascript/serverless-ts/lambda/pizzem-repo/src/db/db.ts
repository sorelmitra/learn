
export type DbOptions = {
	table: string;
	filter?: {
		name: string;
		value: string;
	};
	data?: DbModel;
	id?: string;
	patch?: {
		id: {
			key: string;
			value: string;
		};
		data: any;
	}
};

export type DbError = {
	error: any;
};

export type DbModel = Pizza | Order | DbError;

export interface Db {
	getAll(options: DbOptions): Promise<DbModel[]>;
	create(options: DbOptions): Promise<DbModel>;
	patch(options: DbOptions): Promise<DbModel>;
	delete(options: DbOptions): Promise<DbModel>;
}

export class DbUtils {
	static convert(keyValues: any): DbModel {
		let r: DbModel | null;
		r = DbUtils.convertToPizza(keyValues);
		if (null !== r) return r;
		r = DbUtils.convertToOrder(keyValues);
		if (null !== r) return r;
		throw `Cannot convert ${keyValues} to any known database models!`;
	}

	private static convertToPizza(keyValues): DbModel | null {
		try {
			let pizza: Pizza = {
				type: keyValues["type"],
				description: keyValues["description"],
			};
			if (undefined === pizza.type) {
				return null;
			}
			return pizza;
		} catch (error) {
			return null;
		}
	}

	private static convertToOrder(keyValues): DbModel | null {
		try {
			let order: Order = {
				id: keyValues["id"],
				pizzaType: keyValues["pizzaType"],
				status: keyValues["status"],
			};
			if (undefined === order.id) {
				return null;
			}
			return order;
		} catch (error) {
			return null;
		}
	}
}