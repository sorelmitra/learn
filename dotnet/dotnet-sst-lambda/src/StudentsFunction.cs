using Amazon.DynamoDBv2;
using Amazon.DynamoDBv2.DataModel;
using Amazon.Lambda.APIGatewayEvents;
using Amazon.Lambda.Core;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace DotNetSstLambda;

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
            return Responder.WithSuccess(null);
        }
        catch (Exception ex)
        {
            return Responder.WithError(description:ex.Message);
        }
    }

    public async Task<APIGatewayHttpApiV2ProxyResponse> List(APIGatewayHttpApiV2ProxyRequest request, ILambdaContext context)
    {
        try
        {
            var tenantId = Tenant.Get(request);
            var dbContext = GetDbContext(tenantId);
            var students = await dbContext.ScanAsync<Student>([]).GetRemainingAsync();
            return Responder.WithSuccess(new StudentListResponse { Students = students.ToArray()});
        }
        catch (Exception ex)
        {
            return Responder.WithError(description:ex.Message);
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
            return Responder.WithSuccess(new StudentResponse { Student = student});
        }
        catch (Exception ex)
        {
            return Responder.WithError(description:ex.Message);
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
            return Responder.WithSuccess(new StudentResponse { Student = student });
        }
        catch (Exception ex)
        {
            return Responder.WithError(description:ex.Message);
        }
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
