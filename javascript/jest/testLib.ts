import dotenv from 'dotenv';
import fs from 'fs';
import jwt_decode from 'jwt-decode';

export type FuncArg = (arg?: any) => any;

export const memoize = (func: FuncArg) => {
  const funcResultCache = new Map();
  return (arg?: any) => {
    const cachedValue = funcResultCache.get(arg);
    if (!cachedValue) {
      if (!arg) {
        funcResultCache.set(arg, func());
      } else {
        funcResultCache.set(arg, func(arg));
      }
    }
    return funcResultCache.get(arg);
  };
};

const configure =  () => {
  const environmentName = process.env['ENV'] ?? 'dev';
  const path = `.${environmentName}.env`;
  const values = dotenv.config({ path });
  return {
    ...values?.parsed,
    ...process.env,
    ENV: environmentName
  };
};

const getStore = memoize(configure);

export const getStoreReader =  () => {
  const store = getStore();
  return (key: string) => {
    return store[key];
  };
};

export const getStoreWriter =  () => {
  const store = getStore();
  return (key: string) => {
    return (value) => {
      store[key] = value;
    };
  };
};


/**
 * Generates a timezone offset string from a given Date object.
 * The offset string is calculated from the local timezone offset relative to UTC.
 *
 * @param {Date} date - The date from which to extract the timezone offset.
 * @return {string} - The formatted timezone offset as a string with a leading sign,
 *                    two-digit hour, and two-digit minute (e.g., "+05:30" or "-03:00").
 */
const getLocalTimezoneISO = (date: Date): string => {
  // Retrieve the timezone offset from the date in minutes.
  const offset = date.getTimezoneOffset();

  // Take the absolute value of the offset (since it may be negative).
  const absOffset = Math.abs(offset);

  // Calculate the number of whole hours in the offset.
  const hours = Math.floor(absOffset / 60);

  // Calculate the remaining minutes after the hour division.
  const minutes = absOffset % 60;

  // Determine the sign of the offset (Western hemisphere is typically negative in JavaScript).
  const sign = offset > 0 ? '-' : '+';

  // Pad the hour and minute values with leading zeros to ensure two-digit formatting.
  const paddedHours = hours.toString().padStart(2, '0');
  const paddedMinutes = minutes.toString().padStart(2, '0');

  // Combine the sign, hours, and minutes into a formatted string.
  return `${sign}${paddedHours}:${paddedMinutes}`;
};

/**
 * Converts a date to an ISO-8601 string with the local timezone offset included.
 * This differs from the standard toISOString method, which returns the time in UTC.
 *
 * @param {Date} date - The date to be converted into an ISO string with local timezone.
 * @return {string} - The ISO string representation of the date adjusted for local timezone.
 */
const toLocalISOString = date => {
  // Get the current timezone offset in minutes.
  const offset = date.getTimezoneOffset();

  // Create a new Date object adjusted by the timezone offset to reflect the local time.
  const newDate = new Date(date.getTime() - offset * 60 * 1000);

  // Generate an ISO string from the new Date object, removing the 'Z' that indicates UTC.
  // Append the local timezone offset to the end of the ISO string.
  return newDate.toISOString().slice(0, -1) + getLocalTimezoneISO(date);
};

export const LOG = (...data: any[]) => {
  const now = toLocalISOString(new Date());
  console.log(`[${now}]`, ...data);
}

export const logWithPrefix = (prefix: string) => (...data: any[]) => LOG(`${prefix}:`, ...data);


const getFilename = () => `.${getStoreReader()('ENV')}-cache.json`;

const cacheRead = () => {
  try {
    const buffer = fs.readFileSync(getFilename());
    const fileContent = buffer.toString();
    return JSON.parse(fileContent);
  } catch (e) {
    return {};
  }
};

const cacheWrite = (cache) => {
  try {
    const data = JSON.stringify(cache, null, 2);
    fs.writeFileSync(getFilename(), data);
  } catch (e) {
    LOG(`Cache error updating:`, e);
  }
};

export const cacheSet = (key) => (value) => {
  const cache = cacheRead();
  cache[key] = value;
  cacheWrite(cache);
  return value;
};

export const cacheGet = (key) => {
  const cache = cacheRead();
  return cache[key];
};

type CacheConfig = {
  key: string;
  arg: any;
  describeFunc: FuncArg;
};


/**
 * Get value from cache based on key.  If not found, call the provided function
 * and cache the result.
 * 
 * Call like this:
 * 
    const dummyFunc = (arg: any) => {
      return `${arg} func`;
    };

    const callAndCache = getCachedCaller({
      key: 'cache key',
      arg: 'dummy',
      describeFunc: (dummyData) => `I'm describing {dummyData}`
    });
    callAndCache(dummyFunc);
 *
 */
export const getCachedCaller = (config: CacheConfig) => {
  return async (func: FuncArg) => {
    let resp = cacheGet(config.key);
    let retrieveResult;
    if (!resp) {
      LOG(`Cache miss key <${config.key}>`);
      if (!config.arg) {
        retrieveResult = await func();
      } else {
        retrieveResult = await func(config.arg);
      }
      cacheSet(config.key)(retrieveResult);
    } else {
      LOG(`Cache hit key <${config.key}>`);
    }
    resp = cacheGet(config.key);
    LOG(`${config.key}: ${config.describeFunc(resp)}`);
    return resp;
  };
};

const getEnv = getStoreReader();
const setEnv = getStoreWriter();

export const KEY_LOGIN_DATA = 'loginData';

export type LoginData = {
  accessToken: string;
  expiryDate: string;
};

export type LoginUser = {
  username: string;
  password: string;
};

export type LoginResponse = {
  accessToken?: string;
  data?: {
    login?: {
      accessToken?: string;
    }
  }
}

export type LoginDriver = (user: LoginUser) => Promise<LoginResponse>;

export type LoginConfig = {
  user: LoginUser;
  driver: LoginDriver;
  skipCache?: boolean;
};

const getAccessTokenFromEnv = () => {
  const loginData = getEnv(KEY_LOGIN_DATA) as LoginData;
  LOG('Got login data from store', loginData);
  return loginData.accessToken;
};

export const getTokenExpirationDate = (decodedToken) => {
  const tokenExpiryMs = decodedToken['exp'];
  if (!tokenExpiryMs) {
    return new Date(Date.now());
  }
  return new Date(tokenExpiryMs * 1000);
};

export const decodeToken = (accessToken) => {
  return jwt_decode(accessToken);
};

export const login = async (config: LoginConfig) => {
  const response = await config.driver(config.user);
  const accessToken = response.accessToken ?? response.data?.login?.accessToken
  if (accessToken) {
    const decodedToken = decodeToken(accessToken);
    const tokenExpiryDate = getTokenExpirationDate(decodedToken);
    const loginData: LoginData = {
      accessToken,
      expiryDate: tokenExpiryDate.toISOString()
    };
    if (!config.skipCache) {
      cacheSet(config.user.username)(loginData);
    }
    LOG('Adding login data in store', loginData);
    setEnv(KEY_LOGIN_DATA)(loginData);
  } else {
    setEnv(KEY_LOGIN_DATA)({
      accessToken: 'NO ACCESS TOKEN found, tried <accessToken> and <data.login.accessToken>',
      expiryDate: new Date(Date.now()).toISOString()
    });
  }
  return response;
};

export const dateDiffInMinutes = (aDate) => {
  return (dateToSubtract) => {
    const diffMs = aDate - dateToSubtract;
    return Math.round(((diffMs % 86400000) % 3600000) / 60000);
  };
};

const buildLoginDataFromCachedValues = (username) => {
  const userCachedData = cacheGet(username) as LoginData;
  if (!userCachedData) {
    LOG(`Cache miss key <${username}>, no cached data`);
    return null;
  }
  const tokenExpiryIsoString = userCachedData.expiryDate;
  if (!tokenExpiryIsoString) {
    LOG(`Cache miss key <${username}>, no expiration info`);
    return null;
  }
  const tokenExpiryDate = new Date(tokenExpiryIsoString);
  const minutes = dateDiffInMinutes(tokenExpiryDate)(Date.now());
  if (minutes < 10) {
    LOG(`Cache miss key <${username}>, almost expired`);
    return null;
  }
  LOG(`Cache hit key <${username}>`);
  const loginData: LoginData = {
    accessToken: userCachedData.accessToken,
    expiryDate: tokenExpiryDate.toISOString()
  };
  return loginData;
};

export const loginFromCache = async (config: LoginConfig) => {
  const loginData = buildLoginDataFromCachedValues(config.user.username);
  if (loginData) {
    setEnv(KEY_LOGIN_DATA)(loginData);
    return loginData;
  }
  await login(config);
  return getEnv(KEY_LOGIN_DATA);
};

export const getBearerAuthorizationHeaderValue = () =>
  `Bearer ${getAccessTokenFromEnv()}`;

export const getAuthorizationHeaderValue = () => getAccessTokenFromEnv();

type TestHeaderOptions = { useAccessToken: boolean, skipBearerWord?: boolean };

const addAuthorization = (
  headers: Record<string, any>,
  options?: TestHeaderOptions
) => {
  if (options?.useAccessToken) {
    if (options.skipBearerWord) {
      headers['Authorization'] = getAuthorizationHeaderValue();
    } else {
      headers['Authorization'] = getBearerAuthorizationHeaderValue();
    }
  }
};

export const getGraphQLHeaders = (options?: TestHeaderOptions) => {
  const headers = {
    'Content-Type': 'application/json',
    'apollographql-client-name': getEnv('APOLLO_GRAPHQL_CLIENT_NAME'),
    'apollographql-client-version': getEnv('APOLLO_GRAPHQL_CLIENT_VERSION'),
  };
  addAuthorization(headers, options);
  return headers;
};

export const getHttpHeaders = (options?: TestHeaderOptions) => {
  const headers = {
    'Content-Type': 'application/json'
  };
  addAuthorization(headers, options);
  return headers;
};

export type TestRetrieveCommonConfig = {
  logPrefix?: string;
  method?: string;
  authorize?: boolean;
  skipBearerWord?: boolean;
};

export type TestRetrieveGraphQlConfig = TestRetrieveCommonConfig & {
  query: string;
  variables: Record<string, any>;
};

export type TestRetrieveHttpConfig = TestRetrieveCommonConfig & {
  payload?: any;
};

export type TestJsonType = Record<string, any>;

export const retrieveGraphQl = (url: string) => async (config: TestRetrieveGraphQlConfig): Promise<TestJsonType> => {
  const method = config.method ?? 'POST';
  const prefix = config.logPrefix ?? `Perform a ${method} request`;
  const log = logWithPrefix(`${prefix} to ${url}`);
  const headers = getGraphQLHeaders({
    useAccessToken: config.authorize ?? false,
    skipBearerWord: config.skipBearerWord
  });
  log('headers', headers);
  log('variables', config.variables);
  const payload = {
    query: config.query,
    variables: config.variables
  };
  const response = await fetch(url, {
    method,
    body: JSON.stringify(payload),
    headers
  });
  log('response', response);
  const content = await response.json();
  log('JSON response', JSON.stringify(content, null, 2));
  return content as TestJsonType;
};

export const retrieveHttp = (url: string) => async (config: TestRetrieveHttpConfig): Promise<TestJsonType> => {
  const method = config.method ?? 'GET';
  const prefix = config.logPrefix ?? `Perform a ${method} request`;
  const log = logWithPrefix(`${prefix} to ${url}`);
  const headers = getHttpHeaders({
    useAccessToken: config.authorize ?? false ,
    skipBearerWord: config.skipBearerWord
  });
  log('headers', headers);
  log('payload', config.payload);
  const body = config.payload ? JSON.stringify(config.payload) : undefined;
  const response = await fetch(url, {
    method,
    body,
    headers
  });
  log('response', response);
  const content = await response.json();
  log('JSON response', JSON.stringify(content, null, 2));
  return content as TestJsonType;
};
