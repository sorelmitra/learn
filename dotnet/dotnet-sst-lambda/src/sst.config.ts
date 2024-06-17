import { SSTConfig } from "sst";
import { API } from "./stacks/MyStack";

export default {
  config(_input) {
    return {
      name: "dotnet-sst-lambda",
      region: "us-west-2",
    };
  },
  stacks(app) {
    app.setDefaultFunctionProps({
      runtime: "dotnet8",
      timeout: 30,
    });
    app.stack(API);
  }
} satisfies SSTConfig;
