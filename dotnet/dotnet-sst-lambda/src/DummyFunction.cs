using System.Net;
using Amazon.Lambda.APIGatewayEvents;
using Amazon.Lambda.Core;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace DotNetSstLambda;

public class DummyInput
{
    public string Title { get; set; } = "";
    public int? Code { get; set; }
}

public class ExceptionWithReason(string message, HttpStatusCode statusCode, string reason) : Exception(message)
{
    public HttpStatusCode StatusCode { get; } = statusCode;
    public string Reason { get; } = reason;
}

public class DummyFunction
{
    public APIGatewayHttpApiV2ProxyResponse Handler(APIGatewayHttpApiV2ProxyRequest request, ILambdaContext context)
    {
        try
        {
            var dummyInput = Request.DeserializeBody<DummyInput>(request);
            if (dummyInput.Code == null)
            {
                throw new ExceptionWithReason("missing Code in input!", HttpStatusCode.BadRequest, ErrorResponse.ReasonInvalidArg);
            }
            var dummyValue = new DummyValue
            {
                Title = dummyInput.Title.ToUpper(),
                Code = dummyInput.Code + 1
            };
            return Responder.WithSuccess(dummyValue);
        }
        catch (ExceptionWithReason ex)
        {
            return Responder.WithError(ex.StatusCode, ex.Reason, ex.Message);
        }
        catch (Exception ex)
        {
            return Responder.WithError(HttpStatusCode.InternalServerError, ErrorResponse.ReasonInternal, ex.Message);
        }
    }
}
