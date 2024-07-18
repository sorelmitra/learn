using Amazon.DynamoDBv2;
using Amazon.DynamoDBv2.DataModel;
using Amazon.DynamoDBv2.DocumentModel;
using Amazon.Lambda.APIGatewayEvents;
using Amazon.Lambda.Core;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace DotNetSstLambda;

public interface IResponse
{
    public bool Success { get; set; }
}

public class ErrorResponse: IResponse
{
    public bool Success { get; set; } = false;
    public string? Reason { get; set; }
}

public class SuccessResponse : IResponse
{
    public bool Success { get; set; } = true;
}

public class StudentListResponse: IResponse
{
    public bool Success { get; set; } = true;
    public Student[]? Students { get; set; }
}

public class StudentResponse: IResponse
{
    public bool Success { get; set; } = true;
    public Student? Student { get; set; }
}

[DynamoDBTable("students")]
public class Student
{
    [DynamoDBHashKey("id")]
    public string Id { get; set; } = "";

    [DynamoDBProperty("name")]
    public string Name { get; set; } = "";

    public static Student CreateFromInput(CreateStudentInput input)
    {
        return new Student
        {
            Id = Guid.NewGuid().ToString(),
            Name = input.Name
        };
    }
}

public class CreateStudentInput
{
    public string Name = "";
}

public class StudentsFunction
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
            if (tenantId != Tenant.Known[1])
            {
                throw new Exception("Can only purge on the AAT tenant");
            }
            var dbContext = GetDbContext(tenantId);
            var students = await dbContext.ScanAsync<Student>(new List<ScanCondition>()).GetRemainingAsync();
            var batchWork = dbContext.CreateBatchWrite<Student>();
            batchWork.AddDeleteItems(students);
            await batchWork.ExecuteAsync();
            return RespondWithSuccess(new SuccessResponse());
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
            var dbContext = GetDbContext(tenantId);
            var students = await dbContext.ScanAsync<Student>([]).GetRemainingAsync();
            return RespondWithSuccess(new StudentListResponse { Students = students.ToArray()});
        }
        catch (Exception ex)
        {
            return RespondWithError(ex);
        }
    }

    public async Task<APIGatewayHttpApiV2ProxyResponse> Add(APIGatewayHttpApiV2ProxyRequest request, ILambdaContext context)
    {
        try
        {
            var tenantId = Tenant.Get(request);
            var createStudentInput = Request.DeserializeBody<CreateStudentInput>(request);
            var student = Student.CreateFromInput(createStudentInput);
            var dbContext = GetDbContext(tenantId);
            await dbContext.SaveAsync(student);
            return RespondWithSuccess(new StudentResponse { Student = student});
        }
        catch (Exception ex)
        {
            return RespondWithError(ex);
        }
    }

    public async Task<APIGatewayHttpApiV2ProxyResponse> GetById(APIGatewayHttpApiV2ProxyRequest request, ILambdaContext context)
    {
        try
        {
            var tenantId = Tenant.Get(request);
            var id = Request.GetPathParamValue(request, "student-id");
            var dbContext = GetDbContext(tenantId);
            var student = await dbContext.LoadAsync<Student>(id);
            if (student == null)
            {
                throw new Exception($"No student found for ID {id}!");
            }
            return RespondWithSuccess(new StudentResponse { Student = student });
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
        return new APIGatewayHttpApiV2ProxyResponse
        {
            StatusCode = 500,
            IsBase64Encoded = false,
            Body = JsonConvert.SerializeObject(new ErrorResponse()
            {
                Reason = "Error: " + ex.Message
            }, _serializerSettings),
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
