import {Api, Stack, StackContext, Table, TableProps, use} from "sst/constructs";
import {AuthStack} from "./AuthStack";

const getCustomDomainValues = (stack: Stack) => {
  const externalStage = process.env.EXTERNAL_STAGE || stack.stage;
  const stagePrefix = externalStage === 'prod' ? '' : `${externalStage}.`;
  const hostedZone = `${stagePrefix}goodleap.com`;
  return {externalStage, hostedZone};
};

export function apiDummy({ stack }: StackContext) {
  const {externalStage, hostedZone} = getCustomDomainValues(stack);
  const apiDummy = new Api(stack, "api-dummy", {
    routes: {
      "GET /": "DotNetSstLambda::DotNetSstLambda.DummyFunction::Handler",
    },
    customDomain: {
      domainName: `dummy-service${
          stack.stage === externalStage ? '' : '-' + stack.stage
      }.${hostedZone}`,
      hostedZone: hostedZone,
    },
  });

  stack.addOutputs({
    ApiEndpoint: apiDummy.url,
    CustomDomainUrl: (apiDummy as unknown as { customDomainUrl: string }).customDomainUrl || '',
  });
}

export function apiStudents({ stack }: StackContext) {
  const {externalStage, hostedZone} = getCustomDomainValues(stack);
  const { authorizer } = use(AuthStack);
  const apiStudents = new Api(stack, "api-students", {
    authorizers: {
      LambdaAuthorizer: {
        type: 'lambda',
        function: authorizer,
        responseTypes: ['simple'],
        resultsCacheTtl: '30 seconds',
      },
    },
    defaults: {
      authorizer: "LambdaAuthorizer",
      function: {
        environment: {
          ENV: process.env['ENV']!
        },
        permissions: ['dynamodb:BatchWriteItem', 'dynamodb:DescribeTable', 'dynamodb:DeleteItem', 'dynamodb:GetItem', 'dynamodb:GetRecords', 'dynamodb:PutItem', 'dynamodb:Query', 'dynamodb:Scan', 'dynamodb:UpdateItem']
      },
    },
    routes: {
      "GET /{tenant-id}": "DotNetSstLambda::DotNetSstLambda.StudentsFunction::List",
      "POST /{tenant-id}": "DotNetSstLambda::DotNetSstLambda.StudentsFunction::Add",
      "GET /{tenant-id}/{student-id}": "DotNetSstLambda::DotNetSstLambda.StudentsFunction::GetById",
      "POST /{tenant-id}/purge": "DotNetSstLambda::DotNetSstLambda.StudentsFunction::Purge",
    },
    customDomain: {
      domainName: `dummy-students-service${
          stack.stage === externalStage ? '' : '-' + stack.stage
      }.${hostedZone}`,
      hostedZone: hostedZone,
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
  const defaultTable = new Table(stack, `default-${tableUnprefixedName}`, tableConfig);
  const aatTable = new Table(stack, `aat-${tableUnprefixedName}`, tableConfig);

  stack.addOutputs({
    ApiEndpoint: apiStudents.url,
    DefaultTableName: defaultTable.tableName,
    AatTableName: aatTable.tableName,
    CustomDomainUrl: (apiStudents as unknown as { customDomainUrl: string }).customDomainUrl || '',
  });
}
