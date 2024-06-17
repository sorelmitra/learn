import { StackContext, Api, EventBus } from "sst/constructs";

export function API({ stack }: StackContext) {
  // const bus = new EventBus(stack, "bus", {
  //   defaults: {
  //     retries: 10,
  //   },
  // });

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

  // const apiDummy = new Api(stack, "api-student", {
  //   defaults: {
  //     function: {
  //     },
  //   },
  //   routes: {
  //     "GET /": "DotNetSstLambda::DotNetSstLambda.Students::Handler",
  //   },
  // });

  // bus.subscribe("todo.created", {
  //   handler: "packages/functions/src/events/todo-created.handler",
  // });

  stack.addOutputs({
    ApiEndpoint: apiDummy.url,
  });
}
