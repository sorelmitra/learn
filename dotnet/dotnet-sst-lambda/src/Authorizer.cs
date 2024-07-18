using System.Text.RegularExpressions;
using Amazon.Lambda.APIGatewayEvents;

namespace DotNetSstLambda;

public class Authorizer
{
    public async Task<APIGatewayCustomAuthorizerV2SimpleResponse> Authorize(APIGatewayCustomAuthorizerV2Request lambdaEvent)
    {
        var authHeaderValue = lambdaEvent.IdentitySource[0];
        Console.WriteLine($"Request identity source: {authHeaderValue}");
        var token = new Regex(@"Bearer\s+").Replace(authHeaderValue, replacement:"", count: 1, startat: 0);
        Console.WriteLine($"Token: {token}");
        var response = new APIGatewayCustomAuthorizerV2SimpleResponse
        {
            IsAuthorized = token.Equals("Legit")
        };
        return response;
    }
}
