
export type DbOptions = {
	table: string;
	filter?: {
		name: string;
		value: string;
	}
};

export type DbError = {
	error: any;
};

export type DbModel = Pizza | DbError;

export interface Db {
	getAll(options: DbOptions): Promise<DbModel[]>;
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