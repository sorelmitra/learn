using Amazon.Lambda.APIGatewayEvents;

namespace DotNetSstLambda;

public abstract class Tenant
{
    public static string Get(APIGatewayHttpApiV2ProxyRequest request)
    {
        var tenantId = request.PathParameters["tenant-id"];
        Console.WriteLine($"Tenant {tenantId}");
        return tenantId!;
    }
}
