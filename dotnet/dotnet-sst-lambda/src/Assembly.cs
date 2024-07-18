using Amazon.Lambda.Core;

// Assembly attribute to enable conversion between Lambda function's JSON input/output and .NET classes
// the CamelCaseLambdaJsonSerializer makes sure to use JSON default case, which is Camel
[assembly: LambdaSerializer(typeof(Amazon.Lambda.Serialization.SystemTextJson.CamelCaseLambdaJsonSerializer))]
