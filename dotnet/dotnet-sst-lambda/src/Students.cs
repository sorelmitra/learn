using Amazon.DynamoDBv2;
using Amazon.DynamoDBv2.DataModel;
using Amazon.Lambda.APIGatewayEvents;
using Amazon.Lambda.Core;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace DotNetSstLambda;

public class Response
{
    public bool Success { get; set; }
    public string? Reason { get; set; }
    public object? Payload { get; set; }
}

[DynamoDBTable("students")]
public class Student
{
    [DynamoDBHashKey("id")]
    public string Id { get; set; } = "";

    [DynamoDBProperty("name")]
    public string Name { get; set; } = "";
}

public class Students
{
    private readonly JsonSerializerSettings _serializerSettings = new JsonSerializerSettings
    {
        ContractResolver = new CamelCasePropertyNamesContractResolver(),
        NullValueHandling = NullValueHandling.Ignore
    };

    private readonly AmazonDynamoDBClient _client = new ();

    public async Task<APIGatewayHttpApiV2ProxyResponse> Purge(APIGatewayHttpApiV2ProxyRequest request,
        ILambdaContext context)
    {
        try
        {
            var tenantId = Tenant.Get(request);
            var dbContext = GetDbContext(tenantId);
            var readModels = await dbContext.ScanAsync<Student>(new List<ScanCondition>()).GetRemainingAsync();
            var batchWork = dbContext.CreateBatchWrite<Student>();
            batchWork.AddDeleteItems(readModels);
            await batchWork.ExecuteAsync();
            return RespondWithSuccess(null);
        }
        catch (Exception ex)
        {
            return RespondWithError(ex);
        }
    }

    public async Task<APIGatewayHttpApiV2ProxyResponse> List(APIGatewayHttpApiV2ProxyRequest request, ILambdaContext context)
    {
        try
        {
            var tenantId = Tenant.Get(request);
            return RespondWithSuccess(new Student [] { new Student {} });
        }
        catch (Exception ex)
        {
            return RespondWithError(ex);
        }
    }

    private APIGatewayHttpApiV2ProxyResponse RespondWithSuccess(object? payload)
    {
        return new APIGatewayHttpApiV2ProxyResponse
        {
            StatusCode = 200,
            IsBase64Encoded = false,
            Body = JsonConvert.SerializeObject(payload, _serializerSettings),
            Headers = new Dictionary<string, string> { { "Content-Type", "application/json" } }
        };
    }

    private APIGatewayHttpApiV2ProxyResponse RespondWithError(Exception ex)
    {
        var errorResponse = new Response()
        {
            Success = false,
            Reason = "Error: " + ex.Message
        };
        return new APIGatewayHttpApiV2ProxyResponse
        {
            StatusCode = 500,
            IsBase64Encoded = false,
            Body = JsonConvert.SerializeObject(errorResponse, _serializerSettings),
            Headers = new Dictionary<string, string> { { "Content-Type", "application/json" } }
        };
    }

    private DynamoDBContext GetDbContext(string tenantId)
    {
        var environmentName = Environment.GetEnvironmentVariable("ENV")!;
        var dbContext = new DynamoDBContext(_client, new DynamoDBContextConfig
        {
            TableNamePrefix = $"{environmentName}-dotnet-sst-lambda-{tenantId}-"
        });
        return dbContext;
    }
}
