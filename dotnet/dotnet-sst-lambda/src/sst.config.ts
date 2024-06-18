import { SSTConfig } from "sst";
import {apiDummy, apiStudents} from "./stacks/MyStack";

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
    app.stack(apiDummy);
    app.stack(apiStudents);
  }
} satisfies SSTConfig;
