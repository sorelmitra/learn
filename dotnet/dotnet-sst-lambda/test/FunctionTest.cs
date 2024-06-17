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
