import {Api, StackContext, Table, TableProps} from "sst/constructs";

export function apiDummy({ stack }: StackContext) {
  const apiDummy = new Api(stack, "api-dummy", {
    routes: {
      "GET /": "DotNetSstLambda::DotNetSstLambda.Dummy::Handler",
    },
  });

  stack.addOutputs({
    ApiEndpoint: apiDummy.url,
  });
}

export function apiStudents({ stack }: StackContext) {
  // const bus = new EventBus(stack, "bus", {
  //   defaults: {
  //     retries: 10,
  //   },
  // });

  const apiStudents = new Api(stack, "api-students", {
    defaults: {
      function: {
        environment: {
          ENV: process.env['ENV']!
        },
        permissions: ['dynamodb:BatchWriteItem', 'dynamodb:DescribeTable', 'dynamodb:DeleteItem', 'dynamodb:GetRecords', 'dynamodb:PutItem', 'dynamodb:Query', 'dynamodb:Scan']
        // bind: [bus],
      },
    },
    routes: {
      "GET /{tenant-id}": "DotNetSstLambda::DotNetSstLambda.Students::List",
      "POST /{tenant-id}/purge": "DotNetSstLambda::DotNetSstLambda.Students::Purge",
    },
  });

  const tableUnprefixedName = 'students'
  const tableConfig: TableProps = {
    fields: {
      id: "string",
      name: "string",
    },
    primaryIndex: { partitionKey: "id" },
  };
  const aatTable = new Table(stack, `aat-${tableUnprefixedName}`, tableConfig);

  // bus.subscribe("todo.created", {
  //   handler: "packages/functions/src/events/todo-created.handler",
  // });

  stack.addOutputs({
    ApiEndpoint: apiStudents.url,
    AatTableName: aatTable.tableName
  });
}
