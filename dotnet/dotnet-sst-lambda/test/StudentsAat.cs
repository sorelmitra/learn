using System.Net;
using Newtonsoft.Json;
using Xunit;
using Xunit.Abstractions;

namespace DotNetSstLambda.Tests;

class AatConfig
{
    public string Server { get; set; } = "https://localhost:7133";

    private static AatConfig? _config;

    public static AatConfig Read()
    {
        if (_config != null) return _config;

        string jsonContent = File.ReadAllText(@"../../../aat.json");
        _config = JsonConvert.DeserializeObject<AatConfig>(jsonContent)!;
        return _config;
    }
}

// ReSharper disable once ClassNeverInstantiated.Global
public class AatFixture : IDisposable
{
    private static readonly AatConfig Config = AatConfig.Read();
    private readonly HttpClient _httpClient = new();

    public AatFixture()
    {
        var response = _httpClient.Send(new HttpRequestMessage { Method = HttpMethod.Post, RequestUri = new Uri($"{Config.Server}aat/purge") });
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
    }

    public void Dispose()
    {
    }
}

[CollectionDefinition("AatFixture")]
public class AatFixtureCollection : ICollectionFixture<AatFixture>
{
    // This class has no code, and is never created.
    // Its purpose is to be the place to apply [CollectionDefinition]
    // and all the ICollectionFixture<> interfaces.
}

[Collection("AatFixture")]
public class StudentsAat(ITestOutputHelper output)
{
    private static readonly AatConfig Config = AatConfig.Read();
    private readonly HttpClient _httpClient = new() { BaseAddress = new Uri(Config.Server) };

    [Fact]
    public async Task TestSupportedTenants()
    {
        var httpResponseMessage = await _httpClient.GetAsync("/dummyTenant");
        var jsonString = await httpResponseMessage.Content.ReadAsStringAsync();
        output.WriteLine($"Get, using the wrong tenant: {jsonString}");
        var response = JsonConvert.DeserializeObject<ErrorResponse>(jsonString);
        Assert.NotNull(response);
        Assert.False(response.Success);
        Assert.Matches(".*must be one of default,aat.*", response.Reason);
    }

    [Fact]
    public async Task TestCanOnlyPurgeOnAatTenant()
    {
        var httpResponseMessage = await _httpClient.PostAsync("/default/purge", new StringContent(""));
        var jsonString = await httpResponseMessage.Content.ReadAsStringAsync();
        output.WriteLine($"Purge on the default Tenant: {jsonString}");
        var response = JsonConvert.DeserializeObject<ErrorResponse>(jsonString);
        Assert.NotNull(response);
        Assert.False(response.Success);
        Assert.Matches(".*can only purge on the AAT tenant.*", response.Reason);
    }

    [Fact]
    public async Task TestEmptyList()
    {
        var httpResponseMessage = await _httpClient.GetAsync("/aat");
        var jsonString = await httpResponseMessage.Content.ReadAsStringAsync();
        output.WriteLine($"Get All Students: {jsonString}");
        var response = JsonConvert.DeserializeObject<StudentListResponse>(jsonString);
        Assert.NotNull(response);
        Assert.True(response.Success);
        Assert.NotNull(response.Students);
        Assert.Empty(response.Students);
    }

    [Fact]
    public async Task TestCreateAndGet()
    {
        // Create
        var createStudentPayload = new StringContent(
            JsonConvert.SerializeObject(
                    new CreateStudentInput { Name = "Dummy Foo" }));
        var httpResponseMessage = await _httpClient.PostAsync("/aat", createStudentPayload);
        var response = await GetValue(httpResponseMessage, "Create Student");

        // Get
        var studentId = response.Student!.Id;
        httpResponseMessage = await _httpClient.GetAsync($"/aat/{studentId}");
        await GetValue(httpResponseMessage, "Get Student");
        return;

        async Task<StudentResponse> GetValue(HttpResponseMessage httpResponseMessage1, string message)
        {
            var jsonString = await httpResponseMessage1.Content.ReadAsStringAsync();
            output.WriteLine($"{message}: {jsonString}");
            var studentResponse = JsonConvert.DeserializeObject<StudentResponse>(jsonString);
            Assert.NotNull(studentResponse);
            Assert.True(studentResponse.Success);
            Assert.NotNull(studentResponse.Student);
            Assert.Equal("Dummy Foo", studentResponse.Student.Name);
            Assert.True(Guid.TryParse(studentResponse.Student.Id, out _));
            return studentResponse;
        }
    }
}
