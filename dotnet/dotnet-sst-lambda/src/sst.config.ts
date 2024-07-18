import { SSTConfig } from "sst";
import {apiDummy, apiStudents} from "./stacks/MyStack";
import {AuthStack} from "./stacks/AuthStack";

export default {
  config(_input) {
    return {
      name: "dotnet-sst-lambda",
      region: "us-west-2",
    };
  },
  stacks(app) {
    if (process.env['DESTROY_EVERYTHING']) {
      const awsProfile = process.env['AWS_PROFILE'];
      if (app.stage.match(".*prod.*")
        || (awsProfile && awsProfile.match(".*prod.*"))) {
        console.log(`\n\nCowardly refusing to destroy all data for AWS_PROFILE ${awsProfile} and stage ${app.stage}, as at least one of them seems to be referring to production!\n\n`);
      } else {
        console.log(`\n\n\n!!!\n!!!\n!!!\nWARNING: Requested to DESTROY ALL DATA!\nThis action will completely and IRREVERSIBLY DESTROY all DATA created with this app in AWS_PROFILE ${awsProfile} and STAGE ${app.stage}!!!\n!!!\n!!!\n!!!\n\n\n`);
      }
      app.setDefaultRemovalPolicy("destroy");
    }

    app.setDefaultFunctionProps({
      runtime: "dotnet8",
      timeout: 30,
    });

    app.stack(AuthStack);
    app.stack(apiDummy);
    app.stack(apiStudents);
  }
} satisfies SSTConfig;
