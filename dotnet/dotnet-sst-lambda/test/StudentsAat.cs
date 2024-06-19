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
    public async Task TestEmptyList()
    {
        var httpResponseMessage = await _httpClient.GetAsync("/aat");
        var jsonString = await httpResponseMessage.Content.ReadAsStringAsync();
        var response = JsonConvert.DeserializeObject<StudentListResponse>(jsonString);
        Assert.NotNull(response);
        Assert.True(response.Success);
        Assert.Empty(response.Students);
    }

    // [Fact]
    // public async Task TestCreateAndGet()
    // {
    //     var response = await _httpClient.PostAsync("/aat", new StringContent())
    // }
}
