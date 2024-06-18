using System.Text;
using Amazon.Lambda.APIGatewayEvents;
using Xunit;
using Amazon.Lambda.TestUtilities;
using Newtonsoft.Json;
using Xunit.Abstractions;

namespace DotNetSstLambda.Tests;

public class DummyTest(ITestOutputHelper output)
{
    private readonly Dummy _dummy = new();
    private readonly TestLambdaContext _context = new();

    [Fact]
    public void TestReturnsJson()
    {
        var jsonInput = JsonConvert.SerializeObject(
            new DummyInput { Title = "dummy", Code = 4 });
        var encodedBody = Convert.ToBase64String(Encoding.UTF8.GetBytes(jsonInput));
        var apiGatewayProxyResponse = _dummy.Handler(new APIGatewayHttpApiV2ProxyRequest {Body = encodedBody}, _context);
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
        var apiGatewayProxyResponse = _dummy.Handler(new APIGatewayHttpApiV2ProxyRequest {Body = "I am not base64 encoded"}, _context);
        var jsonResponse = apiGatewayProxyResponse.Body;
        var dummyValue = JsonConvert.DeserializeObject<DummyValue>(jsonResponse);
        Assert.NotNull(dummyValue);
        Assert.False(dummyValue.Success);
        Assert.Matches(".*not a valid Base-64.*", dummyValue.Reason);
        Assert.Equal(500, apiGatewayProxyResponse.StatusCode);
    }
}
