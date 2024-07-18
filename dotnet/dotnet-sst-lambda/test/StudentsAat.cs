using System.Net;
using System.Text;
using System.Text.RegularExpressions;
using Newtonsoft.Json;
using Xunit;
using Xunit.Abstractions;

namespace DotNetSstLambda.Tests;

// ReSharper disable once ClassNeverInstantiated.Global
public class AatFixture : IAsyncLifetime
{
    private static readonly AatConfig Config = AatConfig.Get();
    private readonly HttpClient _httpClient = new() {
        BaseAddress = new Uri(Config.Server)
    };

    // This is how you do an async fixture with XUnit.net
    public async Task InitializeAsync()
    {
        _httpClient.DefaultRequestHeaders.Authorization = await AatAuth.GetHeaderValue();
        var response = await _httpClient.PostAsync("/aat/purge", null);
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
    }

    public Task DisposeAsync()
    {
        return Task.CompletedTask;
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
public class StudentsAat(ITestOutputHelper output): IAsyncLifetime
{
    private static readonly AatConfig Config = AatConfig.Get();

#pragma warning disable CS8618 // Uninitialized non-nullable field - XUnit calls InitializeAsync(), and that initializes it
    private HttpClient _httpClient;
#pragma warning restore CS8618 // Uninitialized non-nullable field

    // We're initializing the HTTP client with default request headers that contain a value
    // that's obtained by calling an async method.  Because of this, we need to use
    // lazy initialization.  The above #pragma disable is the best solution I found, as the
    // compiler is unable to detect that the HTTP client variable is actually initialized
    // at runtime by XUnit.net by calling InitializeAsync()
    private readonly Lazy<Task<HttpClient>> _httpClientBuilder = new(async () => new HttpClient {
        BaseAddress = new Uri(Config.Server),
        DefaultRequestHeaders = { Authorization = await AatAuth.GetHeaderValue()}
    });

    // Set the HTTP client to the value we obtain by awaiting on the lazy initialization of our
    // builder
    public async Task InitializeAsync()
    {
        _httpClient = await _httpClientBuilder.Value;
    }

    public Task DisposeAsync()
    {
        return Task.CompletedTask;
    }

    private async Task<StudentResponse> CheckStudentResponse(HttpResponseMessage httpResponseMessage1, string name, string message)
    {
        var jsonString = await httpResponseMessage1.Content.ReadAsStringAsync();
        output.WriteLine($"{message}: {jsonString}");
        var studentResponse = JsonConvert.DeserializeObject<StudentResponse>(jsonString);
        Assert.NotNull(studentResponse);
        Assert.NotNull(studentResponse.Student);
        Assert.Equal(name, studentResponse.Student.Name);
        Assert.True(Guid.TryParse(studentResponse.Student.Id, out _));
        Assert.Equal(HttpStatusCode.OK, httpResponseMessage1.StatusCode);
        return studentResponse;
    }

    [Fact]
    public async Task TestSupportedTenants()
    {
        var httpResponseMessage = await _httpClient.GetAsync("/dummyTenant");
        var jsonString = await httpResponseMessage.Content.ReadAsStringAsync();
        output.WriteLine($"Get, using the wrong tenant: {jsonString}");
        var response = JsonConvert.DeserializeObject<ErrorResponse>(jsonString);
        Assert.NotNull(response);
        Assert.Matches(".*must be one of default,aat.*", response.Description);
        Assert.Equal(ErrorResponse.ReasonInternal, response.Reason);
        Assert.Equal(HttpStatusCode.InternalServerError, httpResponseMessage.StatusCode);
    }

    [Fact]
    public async Task TestCanOnlyPurgeOnAatTenant()
    {
        var httpResponseMessage = await _httpClient.PostAsync("/default/purge", new StringContent(""));
        var jsonString = await httpResponseMessage.Content.ReadAsStringAsync();
        output.WriteLine($"Purge on the default Tenant: {jsonString}");
        var response = JsonConvert.DeserializeObject<ErrorResponse>(jsonString);
        Assert.NotNull(response);
        Assert.Matches(new Regex(".*can only purge on the AAT tenant.*", RegexOptions.IgnoreCase), response.Description);
        Assert.Equal(ErrorResponse.ReasonInternal, response.Reason);
        Assert.Equal(HttpStatusCode.InternalServerError, httpResponseMessage.StatusCode);
    }

    [Fact]
    public async Task TestCreateAndGet()
    {
        // Create
        var createStudentPayload = new StringContent(
            JsonConvert.SerializeObject(
                    new CreateStudentInput { Name = "Dummy Foo" }));
        var httpResponseMessage = await _httpClient.PostAsync("/aat", createStudentPayload);
        var response = await CheckStudentResponse(httpResponseMessage, "Dummy Foo", "Create Student");

        // Get
        var studentId = response.Student!.Id;
        httpResponseMessage = await _httpClient.GetAsync($"/aat/{studentId}");
        await CheckStudentResponse(httpResponseMessage, "Dummy Foo", "Get Student");
    }

    [Fact]
    public async Task TestList()
    {
        // Create
        HttpResponseMessage? httpResponseMessage;
        for (var i = 0; i < 2; i++)
        {
            var name = $"Bar Taz {i + 1}";
            var createStudentPayload = new StringContent(
                JsonConvert.SerializeObject(
                    new CreateStudentInput { Name = name }));
            httpResponseMessage = await _httpClient.PostAsync("/aat", createStudentPayload);
            await CheckStudentResponse(httpResponseMessage, name, "Create Student");
        }

        // Get all students
        httpResponseMessage = await _httpClient.GetAsync("/aat");
        var jsonString = await httpResponseMessage.Content.ReadAsStringAsync();
        output.WriteLine($"Get All Students: {jsonString}");
        var response = JsonConvert.DeserializeObject<StudentListResponse>(jsonString);
        Assert.NotNull(response);
        Assert.NotNull(response.Students);

        HashSet<string> expectedStudents = ["Bar Taz 1", "Bar Taz 2"];
        var actualStudents = new HashSet<string>(response.Students.Select(s => s.Name));
        actualStudents.IntersectWith(expectedStudents);
        Assert.Equal(expectedStudents, actualStudents);

        Assert.Equal(HttpStatusCode.OK, httpResponseMessage.StatusCode);
    }

    [Fact]
    public async Task TestGetWithoutAuthorization()
    {
        // Get current, it should return an error
        _httpClient.DefaultRequestHeaders.Remove("Authorization");
        var httpResponseMessage = await _httpClient.GetAsync($"/aat");

        // Check error response
        Assert.Equal(HttpStatusCode.Unauthorized, httpResponseMessage.StatusCode);
    }

    [Fact]
    public async Task TestGetWithInvalidAuthorization()
    {
        // Get current, it should return an error
        _httpClient.DefaultRequestHeaders.Remove("Authorization");
        _httpClient.DefaultRequestHeaders.Add("Authorization", "Bearer Malicious");
        var httpResponseMessage = await _httpClient.GetAsync($"/aat");

        // Check error response
        Assert.Equal(HttpStatusCode.Forbidden, httpResponseMessage.StatusCode);
    }
}
