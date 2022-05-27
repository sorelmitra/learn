/* eslint-disable */

import { DocumentClient } from 'aws-sdk/clients/dynamodb';
import { databaseService } from './databaseService';

let ddb;
let data: { Items?: [ { key2: string }? ] } = { Items: [] };

jest.mock('aws-sdk/clients/dynamodb', () => {
  return {
    DocumentClient: jest.fn().mockReturnValue({
      query: jest.fn().mockImplementation(params => {
        return {
          promise: jest.fn().mockReturnValue(Promise.resolve(data))
        };
      })
    })
  };
});

describe('database service tests', () => {

  beforeEach(() => {
    jest.clearAllMocks();
    ddb = new DocumentClient();
    data = {};
  });

  describe('get external ID', () => {
    it('should get one key from DynamoDB', async () => {
      data.Items = [ { key2: 'key2' } ];
      const key2 = await databaseService.getKey2('key1');
      expect(ddb.query).toHaveBeenCalledWith({
        TableName: `dummy-data`,
        KeyConditionExpression: 'key1 = :key1',
        ExpressionAttributeValues: { ':key1': 'key1' },
        ProjectionExpression: 'key2'
      });
      expect(key2).toEqual(data.Items![0]!.key2);
    });

    it('should throw error if key is not found', async () => {
      await expect(databaseService.getKey2('key1')).rejects.toThrowError(/No external id found.*/);
    });
  });
});
