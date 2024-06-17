using System.Text;
using Amazon.Lambda.APIGatewayEvents;
using Xunit;
using Amazon.Lambda.TestUtilities;
using Newtonsoft.Json;
using Xunit.Abstractions;

namespace DotNetSstLambda.Tests;

public class FunctionTest(ITestOutputHelper output)
{
    private readonly Function _function = new();
    private readonly TestLambdaContext _context = new();

    [Fact]
    public void TestToUpperFunction()
    {
        var jsonInput = JsonConvert.SerializeObject(
            new DummyInput { Title = "dummy", Code = 4 });
        var encodedBody = Convert.ToBase64String(Encoding.UTF8.GetBytes(jsonInput));
        var apiGatewayProxyResponse = _function.FunctionHandler(new APIGatewayHttpApiV2ProxyRequest {Body = encodedBody}, _context);
        var jsonResponse = apiGatewayProxyResponse.Result.Body;
        var dummyValue = JsonConvert.DeserializeObject<DummyValue>(jsonResponse);
        Assert.NotNull(dummyValue);
        Assert.True(dummyValue.Success);
        Assert.Null(dummyValue.Reason);
        Assert.Equal("DUMMY", dummyValue.Title);
        Assert.Equal(5, dummyValue.Code);
        Assert.Equal(200, apiGatewayProxyResponse.Result.StatusCode);
    }

    [Fact]
    public void TestFunctionErrorsOut()
    {
        var apiGatewayProxyResponse = _function.FunctionHandler(new APIGatewayHttpApiV2ProxyRequest {Body = "I am not base64 encoded"}, _context);
        var jsonResponse = apiGatewayProxyResponse.Result.Body;
        var dummyValue = JsonConvert.DeserializeObject<DummyValue>(jsonResponse);
        Assert.NotNull(dummyValue);
        Assert.False(dummyValue.Success);
        Assert.Matches(".*not a valid Base-64.*", dummyValue.Reason);
        Assert.Equal(500, apiGatewayProxyResponse.Result.StatusCode);
    }
}
