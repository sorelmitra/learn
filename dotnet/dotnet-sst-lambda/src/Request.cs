using Amazon.Lambda.APIGatewayEvents;
using Newtonsoft.Json;

namespace DotNetSstLambda;

public abstract class Request
{
    public static T DeserializeBody<T>(APIGatewayHttpApiV2ProxyRequest request)
    {
        return DeserializeString<T>(request.Body);
    }

    public static string GetPathParamValue(APIGatewayHttpApiV2ProxyRequest request, string name)
    {
        var value = request.PathParameters[name];
        if (string.IsNullOrEmpty(value))
        {
            throw new Exception($"Invalid value {value} for  path parameter <{name}>, must be non-empty or null");
        }
        Console.WriteLine($"Tenant {value}");
        return value!;
    }

    private static T DeserializeString<T>(string jsonString)
    {
        var t = JsonConvert.DeserializeObject<T>(jsonString);
        if (t == null)
        {
            throw new Exception($"Cannot parse JSON body <{jsonString}>");
        }
        return t;
    }
}
