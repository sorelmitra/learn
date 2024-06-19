using Amazon.Lambda.APIGatewayEvents;

namespace DotNetSstLambda;

public abstract class Tenant
{
    public static readonly string[] Known = ["default", "aat"];

    public static string Get(APIGatewayHttpApiV2ProxyRequest request)
    {
        var tenantId = request.PathParameters["tenant-id"];
        if (!Known.Contains(tenantId))
        {
            throw new Exception($"Unknown tenant <{tenantId}>, must be one of {string.Join(',', Known)}");
        }
        Console.WriteLine($"Tenant {tenantId}");
        return tenantId!;
    }
}
