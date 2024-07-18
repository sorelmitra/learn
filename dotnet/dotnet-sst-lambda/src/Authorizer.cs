using System.Text.RegularExpressions;
using Amazon.Lambda.APIGatewayEvents;

namespace DotNetSstLambda;

public partial class Authorizer
{
    public async Task<APIGatewayCustomAuthorizerV2SimpleResponse> Authorize(APIGatewayCustomAuthorizerV2Request authorizerRequest)
    {
        if (authorizerRequest.IdentitySource.Count < 1)
        {
            return new APIGatewayCustomAuthorizerV2SimpleResponse { IsAuthorized = false };
        }

        var authHeaderValue = authorizerRequest.IdentitySource[0];
        Console.WriteLine($"Request identity source: {authHeaderValue}");
        var token = MyRegex().Replace(authHeaderValue, replacement:"", count: 1, startat: 0);
        Console.WriteLine($"Token: {token}");

        return new APIGatewayCustomAuthorizerV2SimpleResponse
        {
            IsAuthorized = token.Equals("Legit")
        };
    }

    [GeneratedRegex(@"Bearer\s+")]
    private static partial Regex MyRegex();
}
