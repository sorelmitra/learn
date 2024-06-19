using Amazon.Lambda.APIGatewayEvents;

namespace DotNetSstLambda;

public abstract class Tenant
{
    private static readonly string[] KnownTenants = ["default", "aat"];

    public static string Get(APIGatewayHttpApiV2ProxyRequest request)
    {
        var tenantId = request.PathParameters["tenant-id"];
        if (!KnownTenants.Contains(tenantId))
        {
            throw new Exception($"Unknown tenant <{tenantId}>, must be one of {string.Join(',', KnownTenants)}");
        }
        Console.WriteLine($"Tenant {tenantId}");
        return tenantId!;
    }
}
