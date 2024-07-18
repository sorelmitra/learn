# Basic SST with .NET Lambda

These are the steps to create a working `dotnet` Lambda with SST that returns a JSON object.

## Prerequisites

Make sure you have .NET installed and its CLI available.

Connect to AWS in the CLI, take note of the profile name, say `dev-profile`.

## Create and Verify the SST Project

Create:

- Run `yarn create sst dotnet-sst-lambda`.
- It will create a directory, placing a working Node.JS lambda in it, with a default home page and a few functional endpoints.

**Note**: All subsequent commands assume you've CD'ed to the created directory, e.g. `cd dotnet-sst-lambda`, and all file paths are relative to it.

Inspect:

- Take a look at `MyStack.ts`, you will see an `API` function that has a few routes.
- One of them should be `/`.
- The others will probably be a `GET` and a `POST` on some path, say `/todo`.

Start:

- Run `AWS_PROFILE=dev-profile yarn dev`.
- Watch for `ApiEndpoint:`, take note of the URL.
- Open the URL in the browser.  It should display a default hello message.
- Now add `/todo` to the URL, it should list some dummy ToDo entries.

Remove:

- Run `AWS_PROFILE=dev-profile yarn run remove` and make sure it finished successfully.

Now that you've verified that the default project works, it's time to switch it to .NET.

## Switch the Project to .NET

Edit `sst.config.ts`, find the `stacks(app)` declaration, and add the following at the **beginning** of it:

```TypeScript
    app.setDefaultFunctionProps({
      runtime: "dotnet8",
      timeout: 30
    })
```

Remove `packages/*`.

## Add .NET Lambda to the Project

Install the template:

- Look for the Lambda Empty Function .NET template: `dotnet new search lambda.EmptyFunction`.
- Install the corresponding package: `dotnet new install Amazon.Lambda.Templates`

Create an empty Lambda:

- CD to `packages` and run `dotnet new lambda.EmptyFunction -n DotNetSstLambda`.
- It will create a directory, placing two projects inside: a working Lambda that echoes its input, along with tests for it.

Create a solution for your project:

- Run `dotnet new sln --name MySolution`.
- List its contents: `dotnet sln MySolution.sln list`.  It should say 'no projects'.

Rearrange the folder structure:

- Create two folders, and move all the files in the project, and the newly created Lambda with its tests in them, so that at the end the folder structure looks like this:

```
MySolution.sln

src
	sst.config.ts
	(all files and folders from the project root)
	DotNetSstLambda.csproj
	Function.cs

test
	DotNetSstLambda.Tests.csproj
	FunctionTests.cs
```

Add items to your solution:

- Add the Lambda project: `dotnet sln MySolution.sln add --in-root src/DotNetSstLambda.csproj`.
- Add the Lambda tests project: `dotnet sln MySolution.sln add --in-root test/DotNetSstLambda.Tests.csproj`.

Fix the tests project:

- Change the dependency path for the main project to reflect the new folder structure.

Clean-up Lambda project:

- Remove `aws-lambda-tools-defaults.json`, as it's used to deploy the Lambda from Visual Studio, and we won't be using it.
- Remove the ReadMe, as it contains Visual Studio-related info.

## Configure the .NET Lambda with SST

- Edit `MyStack.ts`, change the `"GET /"` line to look like this: `"GET /": "DotNetSstLambda::DotNetSstLambda.Function::FunctionHandler",`

## Update the Lambda and Test Code

See `Function.cs` and `../test/FunctionTest.cs`, respectively.

## Test The Whole Thing

Execute unit tests:

- Run `cd test && dotnet test`

Start locally:

- Run `AWS_PROFILE=dev-profile yarn dev`.

Make a request:

- Run `curl -X GET -H "Content-Type: application/json" <URL> -d '{"title": "dummy", "code": 4}'`.
- You should get back `HI THERE`.

Remove:

- Run `AWS_PROFILE=dev-profile yarn run remove` and make sure it finished successfully.

Deploy:

- Run `AWS_PROFILE=dev-profile yarn deploy --stage dev`.

---

---

---

---

---

---

# Advanced Topics

## .NET Lambda with SST and AAT

In order to do Automatic Acceptance Testing (AAT), any software that has a data store will need to support tenants.

Our demo Lambda handles Students, and it has a database (currently DynamoDB, for simplicity).  For the Students management, it offers a REST API.  Based on [this article](https://medium.com/@vivekmadurai/multi-tenancy-in-rest-api-a570d728620c), the recommended approach is to enforce tenant specification upfront, by baking it into the API path.  Therefore, the Students Lambda has the following mapping:

* `/<tenant-id>/<sub-path>` -> `<env>-<tenant-id>-students` DynamoDB table, where `env` could be `dev`, `prod`, etc.

There will be a `default` tenant for general usage, and an `aat` tenant for AATs.  Other tenants are not added, as our purpose is to just demonstrate the separation of AAT data from real-world data.

## Custom URL

See usage of `customDomain` and `CustomDomainUrl` in `MyStack.ts`.  Make sure to start locally like this:

- Run `EXTERNAL_STAGE=dev AWS_PROFILE=dev-profile yarn dev`.

Explanation: The SST's "internal" stage is the one you use under development, e.g. `sorel`.  The `EXTERNAL_STAGE` is the actual AWS stage, e.g. `dev` or `production`.  In `MyStack.ts::getCustomDomainValues()`, these values are used in order to apply your URL in the correct AWS environment, while still maintaining a suffix with your "internal" stage.

## Lambda Authorizer

Add an `authorizers` object to your SST Api, choose a name, e.g. `LambdaAuthorizer`, and under it add its configuration.  Then use your authorizer by name in your `defaults.authorizer` value.  See all these in action in `MyStack.ts::apiStudents()`.

Define your authorizer Lambda as any other .NET Lambda.  If you don't choose a [Payload version](https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html#http-api-lambda-authorizer.example-code), it will automatically choose the latest one (as of this writing, 2.0).  For the 2.0 Payload version, return `"isAuthorized": true/false`, paying attention to case: `"IsAuthorized": true/false` will NOT work, causing API Gateway to respond with 500 Internal Server Error, with no other explanation, and no way to debug.  See al these in action in `Authorizer.cs`.

To test your setup, invoke as below:

Skip `Authorization` header.  API Gateway will return `401 Unauthorized` WITHOUT calling your `Authorizer` Lambda:

`curl -i -X GET -H "Content-Type: application/json" <URL>/default`

Add a recognized `Authorization` header - in this case, send `Legit` instead of a true access token.  API Gateway will call your `Authorizer` Lambda, and it will see your `Legit` value and authorize it.   API Gateway will then subsequently call your Students endpoint.

`curl -i -X GET -H "Content-Type: application/json" -H "Authorization: Bearer Legit" <URL>/default`

Add an unrecognized `Authorization` header - e.g. send `Malicious` instead of a true access token.  API Gateway will call your `Authorizer` Lambda, and it will see your non-`Legit` value and deny it.  API Gateway will no longer call your Students endpoint, and return `403 Forbidden` instead.

`curl -i -X GET -H "Content-Type: application/json" -H "Authorization: Bearer Malicious" <URL>/default`

See all tests in action in `StudentsAat.cs`:

* All tests by default pass `Legit` as an authorization header
* One test specifically tests unauthorized access by passing `Malicious`.
* Another test omits the `Authorization` header.

See also https://stackoverflow.com/a/53508916/6239668
