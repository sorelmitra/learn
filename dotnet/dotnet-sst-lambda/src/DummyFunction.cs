using Amazon.Lambda.APIGatewayEvents;
using Amazon.Lambda.Core;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace DotNetSstLambda;

public class DummyInput
{
    public string Title { get; set; } = "";
    public int? Code { get; set; }
}

public class DummyValue
{
    public bool Success { get; set; }
    public string? Reason { get; set; }
    public string? Title { get; set; }
    public int? Code { get; set; }
}

public class DummyFunction
{
    private readonly JsonSerializerSettings _serializerSettings = new JsonSerializerSettings
    {
        ContractResolver = new CamelCasePropertyNamesContractResolver(),
        NullValueHandling = NullValueHandling.Ignore
    };

    public APIGatewayHttpApiV2ProxyResponse Handler(APIGatewayHttpApiV2ProxyRequest request, ILambdaContext context)
    {
        try
        {
            var dummyInput = Request.DeserializeBody<DummyInput>(request);
            if (dummyInput.Code == null)
            {
                throw new Exception("missing Code in input!");
            }
            var dummyValue = new DummyValue
            {
                Success = true,
                Title = dummyInput.Title.ToUpper(),
                Code = dummyInput.Code + 1
            };
            return new APIGatewayHttpApiV2ProxyResponse
            {
                StatusCode = 200,
                IsBase64Encoded = false,
                Body = JsonConvert.SerializeObject(dummyValue, _serializerSettings),
                Headers = new Dictionary<string, string> { { "Content-Type", "application/json" } }
            };
        }
        catch (Exception ex)
        {
            var dummyValue = new DummyValue
            {
                Success = false,
                Reason = "Error: " + ex.Message
            };
            return new APIGatewayHttpApiV2ProxyResponse
            {
                StatusCode = 500,
                IsBase64Encoded = false,
                Body = JsonConvert.SerializeObject(dummyValue, _serializerSettings),
                Headers = new Dictionary<string, string> { { "Content-Type", "application/json" } }
            };
        }
    }
}
