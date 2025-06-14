/*

Usage:

AWS_PROFILE=test-profile REGION=us-west-2 QUEUE_URLS=http://localhost:9334/queue/queue-local.fifo,http://localhost:9344/queue/queue-local.fifo npx nodemon aat/tools/local-sns.js

One-time setup:

aws configure set aws_access_key_id "dummy" --profile test-profile && aws configure set aws_secret_access_key "dummy" --profile test-profile && aws configure set region "us-west-2" --profile test-profile

Send an SNS message locally:

aws --region=us-west-2 --endpoint-url=http://localhost:3000/topic sns publish --topic-arn arn:aws:sns:us-east-1:123456789012:test-topic --message-deduplication-id 'dedupe1' --message-group-id 'group1' --message-attributes file://sls-offline-payloads/sns-data-payment-intent-attributes.json --message file://sls-offline-payloads/sns-data-payment-intent-message.json

*/

const express = require('express');
const bodyParser = require('body-parser');
const util = require('util');
const { v4: uuid } = require('uuid');
const { SQSClient, SendMessageCommand } = require('@aws-sdk/client-sqs');

if (!process.env.REGION) throw new Error('Missing REGION env var: should contain the name of the AWS region!');
// if (!process.env.SQS_ENDPOINT) throw new Error('Missing SQS_ENDPOINT env var: should contain the full URL of the AWS SQS endpoint!');
if (!process.env.QUEUE_URLS) throw new Error('Missing QUEUE_URLS env var: should contain a comma-separated list of full URLs of AWS SQS queue where to send the messages!');
console.log(`Using region ${process.env.REGION}, Queue URLs ${process.env.QUEUE_URLS}`);

const sqsClient = new SQSClient({
  region: process.env.REGION,
  // endpoint: process.env.SQS_ENDPOINT,
});

const logLevel = 1;
const debug = (...args) => {if (logLevel > 3) console.timeLog('runtime', '[DEBUG]', ...args)};
const info = (...args) => {if (logLevel > 0) console.timeLog('runtime', '[INFO]', ...args)};
console.time('runtime');

const app = express();

app.use(bodyParser.urlencoded({ extended: true }));

app.post('/topic', async (req, res) => {
  // Entry log
  info('Topic receive: START');

  // Quick debug
  debug('Received', util.inspect(req.body));
  debug('Message', req.body.Message);

  // Parse message attributes
  const attributes = {};
  for (let i = 1; ; i++) {
    const attributeName = req.body[`MessageAttributes.entry.${i}.Name`];
    if (!attributeName) break;
    const attributeDataType = req.body[`MessageAttributes.entry.${i}.Value.DataType`];
    const attributeValueKey = `${attributeDataType}Value`;
    const attributeValue = {
      DataType: attributeDataType,
      Value: req.body[`MessageAttributes.entry.${i}.Value.${attributeValueKey}`]
    };
    attributeValue[attributeValueKey] = attributeValue.Value;
    attributes[attributeName] = attributeValue;
  }
  debug('Attributes', util.inspect(attributes));

  // Send to SQS
  const queueUrls = process.env.QUEUE_URLS.split(',');
  for (const queueUrl of queueUrls) {
    info('SQS Send to', queueUrl);
    const messageGroupId = uuid();
    const commandInput = {
      QueueUrl: queueUrl,
      MessageBody: JSON.stringify({
        Message: req.body.Message,
        MessageAttributes: attributes,
      }),
      MessageGroupId: messageGroupId,
      MessageDeduplicationId: uuid(),
    };
    info('SQS Send Command Input', JSON.stringify({ ...commandInput, MessageBody: { Message: JSON.parse(req.body.Message), MessageAttributes: attributes } }, null, 2));
    const command = new SendMessageCommand(commandInput);
    const response = await sqsClient.send(command);
    info('SQS Send Command Response', util.inspect(response, {depth: 10}));
  }

  // Return fake response
  const messageId = 'abc';
  const requestId = 'def';
  res.send(`
		<PublishResponse xmlns="http://sns.amazonaws.com/doc/2010-03-31/">
			<PublishResult>
			<MessageId>${messageId}</MessageId>
			</PublishResult>
			<ResponseMetadata>
			<RequestId>${requestId}</RequestId>
			</ResponseMetadata>
		</PublishResponse>`
  );

  info('Topic receive: END');
});

const port = 3000;
app.listen(port, () => console.log(`Local SNS is listening on port ${port}.`));
