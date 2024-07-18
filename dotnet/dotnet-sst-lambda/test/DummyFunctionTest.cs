using System.Text;
using Amazon.Lambda.APIGatewayEvents;
using Xunit;
using Amazon.Lambda.TestUtilities;
using Newtonsoft.Json;
using Xunit.Abstractions;

namespace DotNetSstLambda.Tests;

public class DummyFunctionTest(ITestOutputHelper output)
{
    private readonly DummyFunction _dummyFunction = new();
    private readonly TestLambdaContext _context = new();

    [Fact]
    public void TestReturnsJson()
    {
        var apiGatewayProxyResponse = _dummyFunction.Handler(
            new APIGatewayHttpApiV2ProxyRequest
            {
                Body = JsonConvert.SerializeObject(
                    new DummyInput { Title = "dummy", Code = 4 })
            }, _context);
        var jsonResponse = apiGatewayProxyResponse.Body;
        var dummyValue = JsonConvert.DeserializeObject<DummyValue>(jsonResponse);
        Assert.NotNull(dummyValue);
        Assert.True(dummyValue.Success);
        Assert.Null(dummyValue.Reason);
        Assert.Equal("DUMMY", dummyValue.Title);
        Assert.Equal(5, dummyValue.Code);
        Assert.Equal(200, apiGatewayProxyResponse.StatusCode);
    }

    [Fact]
    public void TestErrorsOut()
    {
        var apiGatewayProxyResponse = _dummyFunction.Handler(
            new APIGatewayHttpApiV2ProxyRequest
            {
                Body = JsonConvert.SerializeObject(
                    new DummyInput { Title = "dummy" })
            }, _context);
        var jsonResponse = apiGatewayProxyResponse.Body;
        var dummyValue = JsonConvert.DeserializeObject<DummyValue>(jsonResponse);
        Assert.NotNull(dummyValue);
        Assert.Matches(".*missing Code in input.*", dummyValue.Reason);
        Assert.Equal(500, apiGatewayProxyResponse.StatusCode);
        Assert.False(dummyValue.Success);
    }
}
