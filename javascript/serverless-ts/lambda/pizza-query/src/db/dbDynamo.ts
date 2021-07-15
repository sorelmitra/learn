import { AWSError } from 'aws-sdk';
import { DocumentClient } from 'aws-sdk/clients/dynamodb';
import { PromiseResult } from 'aws-sdk/lib/request';

import { Db, DbModel, DbOptions } from "./db";

class DbDynamo implements Db {
	async getAll(options: DbOptions): Promise<DbModel[]> {
		console.log("DynamoDB entry");
		let values: DbModel[] = [];
		try {
			let r = await this.scanTable(options.table);
			console.log(`Scan ${options.table} results`, r);
			return values;
		} catch (error) {
			throw error;
		}
	}

	async scanTable(tableName) {
		let documentClient = new DocumentClient();
		const params: DocumentClient.ScanInput = {
			TableName: tableName,
		};

		let scanResults: DocumentClient.AttributeMap[] = [];
		let items: PromiseResult<DocumentClient.ScanOutput, AWSError>;
		let i = 0;
		do {
			i++;
			console.log(`Table ${tableName} scan take ${i}`);
			try {
				items = await documentClient.scan(params).promise();
			} catch (error) {
				console.log(`Table ${tableName} scan take ${i} ERROR`, error);
				throw error;
			}
			if (undefined === items.Items) {
				console.log(`Table ${tableName} scan: NO results`);
				return scanResults;
			}
			items.Items.forEach((item) => scanResults.push(item));
			params.ExclusiveStartKey = items.LastEvaluatedKey;
		} while (typeof items.LastEvaluatedKey !== "undefined");

		console.log(`Table ${tableName} scan: ${scanResults.length} results after ${i} scans`);
		return scanResults;
	};
}

export default DbDynamo;
