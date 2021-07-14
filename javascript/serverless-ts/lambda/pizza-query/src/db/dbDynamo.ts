import { AWSError } from 'aws-sdk';
import { DocumentClient, BatchGetItemInput } from 'aws-sdk/clients/dynamodb';
import { PromiseResult } from 'aws-sdk/lib/request';

import { Db, DbModel, DbOptions } from "./db";

class DbDynamo implements Db {
	async getAll(options: DbOptions): Promise<DbModel[]> {
		console.log("DynamoDB entry");
		let values: DbModel[] = [];
		let documentClient = new DocumentClient();
		let params: BatchGetItemInput = {
			RequestItems: {}
		};
		params.RequestItems[options.table] = {
			Keys: []
		};
		let res: PromiseResult<DocumentClient.BatchGetItemOutput, AWSError>;
		console.log("DynamoDB get");
		res = await documentClient.batchGet(params).promise();
		console.log("DynamoDB data", res);
		return values;
	}
}

export default DbDynamo;
