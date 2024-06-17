using Amazon.Lambda.APIGatewayEvents;
using Amazon.Lambda.Core;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

// Assembly attribute to enable the Lambda function's JSON input to be converted into a .NET class.
[assembly: LambdaSerializer(typeof(Amazon.Lambda.Serialization.SystemTextJson.DefaultLambdaJsonSerializer))]

namespace DotNetSstLambda;

public class DummyInput
{
    public string Title { get; set; } = "";
    public int Code { get; set; }
}

public class DummyValue
{
    public bool Success { get; set; }
    public string? Reason { get; set; }
    public string? Title { get; set; }
    public int? Code { get; set; }
}

public class Function
{
    private readonly JsonSerializerSettings _serializerSettings = new JsonSerializerSettings
    {
        ContractResolver = new CamelCasePropertyNamesContractResolver(),
        NullValueHandling = NullValueHandling.Ignore
    };

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
            var jsonString = System.Text.Encoding.UTF8.GetString(base64EncodedBytes);
            var dummyInput = JsonConvert.DeserializeObject<DummyInput>(jsonString);
            if (dummyInput == null)
            {
                throw new Exception($"Cannot parse JSON body <{jsonString}>");
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
