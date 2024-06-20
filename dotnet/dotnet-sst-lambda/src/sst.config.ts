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
    if (process.env['DESTROY_EVERYTHING'] && app.stage !== "prod") {
      console.log(`\n\n\n!!!\n!!!\n!!!\nWARNING: Requested to DESTROY ALL DATA!\nThis action will completely and IRREVERSIBLY DESTROY all DATA created with this app in STAGE ${app.stage}!!!\n!!!\n!!!\n!!!\n\n\n`);
      app.setDefaultRemovalPolicy("destroy");
    }
    app.setDefaultFunctionProps({
      runtime: "dotnet8",
      timeout: 30,
    });
    app.stack(apiDummy);
    app.stack(apiStudents);
  }
} satisfies SSTConfig;
