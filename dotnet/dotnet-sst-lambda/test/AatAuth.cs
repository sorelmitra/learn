using System.Net.Http.Headers;

namespace DotNetSstLambda.Tests;

public abstract class AatAuth {
#pragma warning disable CS1998 // We're simulating an async method here - Async method lacks 'await'
	public static async Task<AuthenticationHeaderValue> GetHeaderValue()
#pragma warning restore CS1998 // Async method lacks 'await'
	{
		return new AuthenticationHeaderValue("Bearer", "Legit");
	}
}
