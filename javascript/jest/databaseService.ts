import { DocumentClient } from 'aws-sdk/clients/dynamodb';

interface IDatabaseService {
  getKey2(key1: string): Promise<string>;
}

class DatabaseService implements IDatabaseService {
  private ddb: DocumentClient = new DocumentClient();

  async getKey2(key1: string): Promise<string> {
    try {
      const { Items: items } = await this.ddb
        .query({
          TableName: `dummy-data`,
          KeyConditionExpression: 'key1 = :key1',
          ExpressionAttributeValues: { ':key1': key1 },
          ProjectionExpression: 'key2'
        })
        .promise();
      if (!items || items.length === 0) {
        throw new Error(`Key 2 not found for key 1 ${key1}!`);
      }
      return items[0].key2 as string;
    } catch (err) {
      return Promise.reject(err);
    }
  }
}

export const databaseService: IDatabaseService = new DatabaseService();
