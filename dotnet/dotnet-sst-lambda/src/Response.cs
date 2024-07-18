using System.Net;
using Amazon.DynamoDBv2.DataModel;
using Amazon.Lambda.APIGatewayEvents;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace DotNetSstLambda;

public class DummyValue
{
    public string? Title { get; set; }
    public int? Code { get; set; }
}

public class StudentListResponse
{
    public Student[]? Students { get; set; }
}

public class StudentResponse
{
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

public class ErrorResponse
{
    public const string ReasonInternal = "INTERNAL_SERVER_ERROR";
    public const string ReasonInvalidArg = "INVALID_ARGUMENT";

    public string? Reason { get; init; }
    public string? Description { get; init; }
}

public abstract class Responder
{
    private static readonly JsonSerializerSettings SerializerSettings = new JsonSerializerSettings
    {
        ContractResolver = new CamelCasePropertyNamesContractResolver(),
        NullValueHandling = NullValueHandling.Ignore
    };

    public static APIGatewayHttpApiV2ProxyResponse WithSuccess(object? payload, HttpStatusCode statusCode = HttpStatusCode.OK)
    {
        return new APIGatewayHttpApiV2ProxyResponse
        {
            StatusCode = (int)statusCode,
            IsBase64Encoded = false,
            Body = JsonConvert.SerializeObject(payload, SerializerSettings),
            Headers = new Dictionary<string, string> { { "Content-Type", "application/json" } }
        };
    }

    public static APIGatewayHttpApiV2ProxyResponse WithError(HttpStatusCode statusCode = HttpStatusCode.InternalServerError, string reason = ErrorResponse.ReasonInternal, string description="An internal server error has occured")
    {
        return new APIGatewayHttpApiV2ProxyResponse
        {
            StatusCode = (int)statusCode,
            IsBase64Encoded = false,
            Body = JsonConvert.SerializeObject(new ErrorResponse()
            {
                Reason = reason,
                Description = description
            }, SerializerSettings),
            Headers = new Dictionary<string, string> { { "Content-Type", "application/json" } }
        };
    }
}