
export type DbOptions = {
	table: string;
	filter?: {
		name: string;
		value: string;
	};
	data?: DbModel;
	id?: string;
};

export type DbError = {
	error: any;
};

export type DbModel = Pizza | Order | DbError;

export interface Db {
	getAll(options: DbOptions): Promise<DbModel[]>;
	create(options: DbOptions): Promise<DbModel>;
	delete(options: DbOptions): Promise<DbModel>;
}

export class DbUtils {
	static convert(keyValues: any): DbModel {
		let r = DbUtils.convertToPizza(keyValues);
		if (null !== r) return r;
		throw `Cannot convert ${keyValues} to any known database models!`;
	}

	private static convertToPizza(keyValues): DbModel | null {
		try {
			return {
				type: keyValues["type"],
				description: keyValues["description"],
			};
		} catch (error) {
			return null;
		}
	}
}