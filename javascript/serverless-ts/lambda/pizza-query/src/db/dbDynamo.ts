import { AWSError } from 'aws-sdk';
import { DocumentClient } from 'aws-sdk/clients/dynamodb';
import { PromiseResult } from 'aws-sdk/lib/request';

import { Db, DbModel, DbOptions, DbUtils } from "./db";

class DbDynamo implements Db {
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
		let documentClient = new DocumentClient();
		const params: DocumentClient.ScanInput = {
			TableName: options.table,
		};

		if (options.filter) {
			params.FilterExpression = `#type = :this_type`;
			params.ExpressionAttributeNames = {};
			params.ExpressionAttributeNames[`#type`] = `type`;
			params.ExpressionAttributeValues = {
				":this_type": options.filter.value
			};
		}

		let scanResults: DocumentClient.AttributeMap[] = [];
		let items: PromiseResult<DocumentClient.ScanOutput, AWSError>;
		let i = 0;
		do {
			i++;
			console.log(`Table ${options.table} scan take ${i}`);
			try {
				items = await documentClient.scan(params).promise();
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
}

export default DbDynamo;
