/**
 * An (environment variable, regular expression) tuple.
 */
export type EnvVarAndRegex = {
  name: string;
  regex: RegExp;
};

/**
 * Return `jest.it` if any of the received env vars matches its regex.
 * Return `jest.skip` otherwise.
 * @param envVarAndRegexes List of (env var, regex) tuples.
 */
export const runOnlyIfAnyEnvVarMatchesItsRegex = (envVarAndRegexes?: EnvVarAndRegex[]) => {
  if (!envVarAndRegexes || envVarAndRegexes.length < 1) return it;
  for (const envVarAndRegex of envVarAndRegexes) {
    const envVar = process.env[envVarAndRegex.name];
    if (!envVar) continue;
    if (envVar.match(envVarAndRegex.regex)) return it;
  }
  return it.skip;
};

describe('payment-party-service', () => {
  // Run this test only if my server runs on localhost
  runOnlyIfAnyEnvVarMatchesItsRegex([{
    name: 'MY_SERVER',
    regex: aatGetLocalhostRegex(),
  }])('should test a feature', async () => {
    expect('myTest passes').toBeTruthy();
  });
});

