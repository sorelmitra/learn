import { StackContext, Api, EventBus } from "sst/constructs";

export function apiDummy({ stack }: StackContext) {
  const apiDummy = new Api(stack, "api-dummy", {
    defaults: {
      function: {
        // bind: [bus],
      },
    },
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
      },
    },
    routes: {
      "GET /": "DotNetSstLambda::DotNetSstLambda.Students::Handler",
    },
  });

  // bus.subscribe("todo.created", {
  //   handler: "packages/functions/src/events/todo-created.handler",
  // });

  stack.addOutputs({
    ApiEndpoint: apiStudents.url,
  });
}
