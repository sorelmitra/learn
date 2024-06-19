using Amazon.Lambda.APIGatewayEvents;
using Newtonsoft.Json;

namespace DotNetSstLambda;

public abstract class Request
{
    public static T DeserializeBase64Body<T>(APIGatewayHttpApiV2ProxyRequest request)
    {
        var base64EncodedBytes = Convert.FromBase64String(request.Body);
        var jsonString = System.Text.Encoding.UTF8.GetString(base64EncodedBytes);
        return DeserializeString<T>(jsonString);
    }

    public static T DeserializeBody<T>(APIGatewayHttpApiV2ProxyRequest request)
    {
        return DeserializeString<T>(request.Body);
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
