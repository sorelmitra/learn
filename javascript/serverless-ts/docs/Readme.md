# Overview

This is an introductory project for the technologies listed in the accompanying PDF document.

# Architecture

You need to create a system for a pizza ordering app. This system should be designed with the following specs in mind:

- I want to be able to query all the pizza types and variants from the database via an API
- Each order must have an email address associated to it, so it can notify the customer of any status change of the order. I want be able to query all the orders that were placed using a certain email.
- I must be notified via email each time the status of my order changes (e.g. order received, order being prepared, order being delivered, etc.)
- A receipt has to be generated and stored in S3 for each order (don’t get fancy with it, a .txt will do). Once generated and saved, a copy of the file has to be sent to the customer’s provided email.

Tech stack: TypeScript, node.js, hapi.js, DynamoDB, DynamoDB Streams, S3, SQS, SNS, Lambda, Serverless Framework, SES

- AWS SNS & SQS: https://stackoverflow.com/a/13692720/6239668
- AWS DynamoDB Streams use cases & more: https://dynobase.dev/dynamodb-streams/
- Node.JS Express vs Hapi: https://www.simform.com/express-vs-hapi/

Data flows:

```
Query Pizza Types -> Lambda pizzemQueryPizza -> DynamoDB -> HTTPS

Query Orders by Email -> Lambda pizzemQueryOrder -> DynamoDB -> HTTPS

Order Change Request -> Lambda pizzemUpdateOrder -> Dynamo DB -> Streams ->
	-> Lambda pizzemChangeRequest (*) -> SNS (*y) ->
		Lambda pizzemEnqueueEmailOrderUpdate -> SQS ->
			-> Lambda pizzemEmailOrderUpdate -> SES -> Send Email
		Lambda pizzemEnqueueSaveReceipt -> SQS -> 
			Lambda pizzemSaveReceipt -> S3 -> Store receipt -> SQS -> 
				-> Lambda pizzemEmailReceipt -> SES -> Send Email
```

(*) As per https://stackoverflow.com/questions/38576679/multiple-aws-lambda-functions-on-a-single-dynamodb-stream, DynamoDB Streams doesn't want more than two lambdas listening on it, so to support extending the system it's a good design decision to have a single Lambda that listens on DynamoDB Streams and publish to SNS.

(*y) SNS might not seem obvious here but it helps resolve an important problem: decoupling the Lambda that notifies of the change from the (possibly many) other Lambdas that do particular processing of that change.