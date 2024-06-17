# SST with .NET Lambda

## Steps to create a working `dotnet` Lambda with SST

### Prerequisites

Make sure you have .NET installed and its CLI available.

Connect to AWS in the CLI, take note of the profile name, say `dev-profile`.

### Create and Verify the SST Project

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

### Switch the Project to .NET

Edit `sst.config.ts`, find the `stacks(app)` declaration, and add the following at the **beginning** of it:

```TypeScript
    app.setDefaultFunctionProps({
      runtime: "dotnet8",
      timeout: 30
    })
```

Remove `packages/*`.

### Add .NET Lambda to the Project

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

### Configure the .NET Lambda with SST

- Edit `MyStack.ts`, change the `"GET /"` line to look like this: `"GET /": "DotNetSstLambda::DotNetSstLambda.Function::FunctionHandler",`

### Update the Lambda Code

```C#
using System.Text.Json;
using Amazon.Lambda.APIGatewayEvents;
using Amazon.Lambda.Core;

// Assembly attribute to enable the Lambda function's JSON input to be converted into a .NET class.
[assembly: LambdaSerializer(typeof(Amazon.Lambda.Serialization.SystemTextJson.DefaultLambdaJsonSerializer))]

namespace DotNetSstLambda;

public class Function
{
    /// <summary>
    /// A simple function that takes a string and does a ToUpper
    /// </summary>
    /// <param name="request">The input for the Lambda function handler to process.</param>
    /// <param name="context">The ILambdaContext that provides methods for logging and describing the Lambda environment.</param>
    /// <returns></returns>
    public async Task<APIGatewayHttpApiV2ProxyResponse> FunctionHandler(APIGatewayHttpApiV2ProxyRequest request, ILambdaContext context)
    {
        try
        {
            var base64EncodedBytes = Convert.FromBase64String(request.Body);
            var message = System.Text.Encoding.UTF8.GetString(base64EncodedBytes);
            return new APIGatewayHttpApiV2ProxyResponse
            {
                StatusCode = 200,
                IsBase64Encoded = false,
                Body = message.ToUpper(),
                Headers = new Dictionary<string, string> { { "Content-Type", "application/json" } }
            };
        }
        catch (Exception ex)
        {
            return new APIGatewayHttpApiV2ProxyResponse
            {
                StatusCode = 500,
                IsBase64Encoded = false,
                Body = "Error: " + ex.Message,
                Headers = new Dictionary<string, string> { { "Content-Type", "application/json" } }
            };
        }
    }
}
```

### Update the Test Code

```C#
using System.Text.Json;
using Amazon.Lambda.APIGatewayEvents;
using Xunit;
using Amazon.Lambda.TestUtilities;
using Xunit.Abstractions;

namespace DotNetSstLambda.Tests;

public class FunctionTest(ITestOutputHelper output)
{
    private readonly Function _function = new();
    private readonly TestLambdaContext _context = new();

    [Fact]
    public void TestToUpperFunction()
    {
        var encodedBody = Convert.ToBase64String("hello world"u8.ToArray());
        var apiGatewayProxyResponse = _function.FunctionHandler(new APIGatewayHttpApiV2ProxyRequest {Body = encodedBody}, _context);
        var message = apiGatewayProxyResponse.Result.Body;
        Assert.Equal("HELLO WORLD", message);
        Assert.Equal(200, apiGatewayProxyResponse.Result.StatusCode);
    }

    [Fact]
    public void TestErrorsOut()
    {
        var apiGatewayProxyResponse = _function.FunctionHandler(new APIGatewayHttpApiV2ProxyRequest {Body = "I am not base64 encoded"}, _context);
        var message = apiGatewayProxyResponse.Result.Body;
        Assert.Matches(".*not a valid Base-64.*", message);
        Assert.Equal(500, apiGatewayProxyResponse.Result.StatusCode);
    }
}
```

### Test The Whole Thing

Execute unit tests:

- Run `cd test && dotnet test`

Start locally:

- Run `AWS_PROFILE=dev-profile yarn dev`.

Make a request:

- Run `curl -X GET <URL> -d "hi there"`.
- You should get back `HI THERE`.

Remove:

- Run `AWS_PROFILE=dev-profile yarn run remove` and make sure it finished successfully.

Deploy:

- Run `AWS_PROFILE=dev-profile yarn deploy --stage dev`.
