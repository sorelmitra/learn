
export type DbOptions = {
	table: string;
};

export type DbModel = Pizza;

export interface Db {
	getAll(options: DbOptions): DbModel[];
}
