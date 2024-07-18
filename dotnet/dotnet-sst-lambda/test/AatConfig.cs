using Newtonsoft.Json;

namespace DotNetSstLambda.Tests;

class AatConfig
{
    public string Server { get; set; } = "https://localhost:7133";

    private static AatConfig? _config;

    public static AatConfig Get()
    {
        if (_config != null) return _config;

        var jsonContent = File.ReadAllText(@"../../../aat.json");
        _config = JsonConvert.DeserializeObject<AatConfig>(jsonContent)!;
        return _config;
    }
}