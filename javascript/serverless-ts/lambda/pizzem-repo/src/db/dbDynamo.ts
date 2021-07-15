import { AWSError } from 'aws-sdk';
import { DocumentClient } from 'aws-sdk/clients/dynamodb';
import { PromiseResult } from 'aws-sdk/lib/request';

import { Db, DbModel, DbOptions, DbUtils } from "./db";

class DbDynamo implements Db {
	documentClient: DocumentClient;

	constructor(documentClient: DocumentClient) {
		this.documentClient = documentClient;
	}

	async getAll(options: DbOptions): Promise<DbModel[]> {
		console.log("DynamoDB entry");
		let values: DbModel[] = [];
		try {
			let r: DocumentClient.AttributeMap[] = await this.scanTable(options);
			console.log(`Scan ${options.table} results`, r);
			r.forEach(item => values.push(DbUtils.convert(item)));
			return values;
		} catch (error) {
			return [{ error: error }];
		}
	}

	async scanTable(options: DbOptions) {
		const params: DocumentClient.ScanInput = {
			TableName: options.table,
		};

		if (options.filter) {
			params.FilterExpression = `#id_field_name = :id_field_value`;
			params.ExpressionAttributeNames = {};
			params.ExpressionAttributeNames[`#id_field_name`] = options.filter.name;
			params.ExpressionAttributeValues = {
				":id_field_value": options.filter.value
			};
		}

		let scanResults: DocumentClient.AttributeMap[] = [];
		let items: PromiseResult<DocumentClient.ScanOutput, AWSError>;
		let i = 0;
		do {
			i++;
			console.log(`Table ${options.table} scan take ${i}`);
			try {
				items = await this.documentClient.scan(params).promise();
			} catch (error) {
				console.log(`Table ${options.table} scan take ${i} ERROR`, error);
				throw error;
			}
			if (undefined === items.Items) {
				console.log(`Table ${options.table} scan: no results`);
				return scanResults;
			}
			items.Items.forEach((item) => scanResults.push(item));
			params.ExclusiveStartKey = items.LastEvaluatedKey;
		} while (typeof items.LastEvaluatedKey !== "undefined");

		console.log(`Table ${options.table} scan: ${scanResults.length} results after ${i} scans`);
		return scanResults;
	};

	async create(options: DbOptions): Promise<DbModel> {
		if (undefined === options.data) {
			return { error: "Missing data to create in the DB!" };
		}
		try {
			let params: DocumentClient.PutItemInput = {
				TableName: options.table,
				Item: {},
			};
			for (let key in options.data) {
				params.Item[key] = options.data[key];
			}
			let r: DocumentClient.PutItemOutput = await this.documentClient.put(params).promise();
			return options.data;
		} catch (error) {
			return { error: error };
		}
	}

	async patch(options: DbOptions): Promise<DbModel> {
		if (undefined === options.patch) {
			return { error: "Missing patch in the DB!" };
		}
		let m: DbModel[] = await this.getAll({
			table: options.table,
			filter: {
				name: options.patch.id.key,
				value: options.patch.id.value,
			}
		});
		console.log("patch find item result", m);
		if (m.length < 1) {
			return { error: `Couldn't find item with id ${options.patch.id.value} in table ${options.table}` };
		}
		let existingItem = m[0];
		for (let key in options.patch.data) {
			existingItem[key] = options.patch.data[key];
		}
		try {
			let params: DocumentClient.PutItemInput = {
				TableName: options.table,
				Item: {},
			};
			for (let key in existingItem) {
				params.Item[key] = existingItem[key];
			}
			console.log("patch item", params.Item);
			let r: DocumentClient.PutItemOutput = await this.documentClient.put(params).promise();
			return existingItem;
		} catch (error) {
			return { error: error };
		}
	}

	async delete(options: DbOptions): Promise<DbModel> {
		if (undefined === options.id) {
			return { error: "Missing data to create in the DB!" };
		}
		try {
			let params: DocumentClient.DeleteItemInput = {
				TableName: options.table,
				Key: {
					id: options.id,
				},
			};
			let r: DocumentClient.DeleteItemOutput = await this.documentClient.delete(params).promise();
			return {
				id: options.id,
				pizzaType: "not implemented",
				status: "not implemented",
			};
		} catch (error) {
			return { error: error };
		}
	}
}

export default DbDynamo;
