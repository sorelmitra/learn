import * as fs from 'fs';
import { HttpException, HttpStatus, Inject, Injectable, Logger } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import * as OAuthClient from 'intuit-oauth';
import { Cache } from 'cache-manager';
import { CACHE_MANAGER } from '@nestjs/cache-manager';

type Token = {
  value: string;
  expiration: Date;
};

@Injectable()
export class IntuitOAuthService {
  endpoint: string;

  constructor(
    private readonly logger: Logger,
    private readonly configService: ConfigService,
    private readonly oAuthClient: OAuthClient,
    @Inject(CACHE_MANAGER) private cache: Cache,
  ) {
    const key = 'INTUIT_V3_ENDPOINT_BASE_URL';
    const baseUrl = configService.get<string>(key);
    if (!baseUrl) {
      throw new Error(`Missing ${key} environment variable!`);
    }
    this.endpoint = baseUrl;
  }

  async performAuthorizationCodeGrant(code: string, realmId?: string, state?: string) {
    const q = (name: string, value?: string) => {
      if (!value) return '';
      return `${name}=${value}`;
    };

    this.logger.debug(
      `Authorization code grant: code ${code}, realm ID ${realmId}, state ${state}`,
    );
    if (!code || !realmId) {
      throw new HttpException('Missing authorization code or realmId', HttpStatus.BAD_REQUEST);
    }
    const callbackUrl = `${this.configService.get('QB_REDIRECT_URI')}${q('?code', code)}${q('&realmId', realmId)}${q('&state', state)}`;
    this.logger.debug(`Callback URL ${callbackUrl}`);
    const resp = await this.oAuthClient.createToken(callbackUrl);
    const tokenData = resp.getJson();

    const refreshToken: Token = {
      value: tokenData['refresh_token'],
      expiration: new Date(
        Date.now() + new Number(tokenData['x_refresh_token_expires_in']).valueOf() * 1000,
      ),
    };
    const accessToken: Token = {
      value: tokenData['access_token'],
      expiration: new Date(Date.now() + new Number(tokenData['expires_in']).valueOf() * 1000),
    };
    this.saveRefreshToken(refreshToken);
    this.saveAccessToken(accessToken);
    this.saveRealmId(realmId);

    return JSON.stringify({
      message: `Successfully connected to Intuit, access token expires on ${accessToken.expiration.toISOString()}, refresh token expires on ${refreshToken.expiration.toISOString()}`,
    });
  }

  /**
   * Makes an API call to the QuickBooks endpoint.
   * @param path the path of the API endpoint to call, must start with a '/'.
   * @param method the http method to use.
   * @param body the request body.
   * @returns the result of the API call.
   *
   * The method first retrieves an access token for the given contractor.
   * If the token is expired, it will be refreshed.
   * The method will then make the API call with the retrieved access token.
   * If the API call fails, the method will throw an error.
   */
  async makeApiCall(path: string, method: string, body?: any): Promise<any> {
    const companyId = this.findRealmId();
    const url = `${this.endpoint}${companyId}${path}`;
    const token = await this.getValidAccessToken();
    try {
      return await this.oAuthClient.makeApiCall({
        url,
        method,
        headers: { Authorization: `Bearer ${token}`, 'Content-Type': 'application/json' },
        body: JSON.stringify(body),
      });
    } catch (e) {
      const error = e as Record<string, any>;
      const message = `Api call to QuickBooks path ${path} failed`;
      this.logger.error(message, 'IntuitOAuthService', {
        url,
        error: error?.error,
        errorResponseData: error?.response?.data,
      });
      throw new HttpException(message, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  /**
   * Checks for a valid cached access token.
   * If it expires in less than 10 minutes, refreshes it using the stored refresh token.
   * If the refresh token expires in less than 1 day, an error is thrown to force re-authorization.
   */
  private async getValidAccessToken(): Promise<string> {
    const TEN_MINUTES = 10 * 60 * 1000;
    const ONE_DAY = 24 * 60 * 60 * 1000;

    // Retrieve the cached access token
    const accessToken = this.findAccessToken();

    // If the access token exists and is valid for more than 10 minutes, return it
    if (accessToken) {
      const timeLeft = new Date(accessToken.expiration).getTime() - Date.now();
      if (timeLeft > TEN_MINUTES) {
        this.logger.log(`Using cached access token, expires in ${timeLeft} ms.`);
        return accessToken.value;
      }
      this.logger.log('Access token expiring soon. Refreshing...');
    } else {
      this.logger.log('No cached access token found. Will attempt to refresh.');
    }

    // Retrieve the refresh token
    const refreshToken = this.findRefreshToken();
    if (!refreshToken) {
      throw new Error('No refresh token available.');
    }

    // Check if the refresh token is expiring in less than 1 day
    const refreshTimeLeft = new Date(refreshToken.expiration).getTime() - Date.now();
    if (refreshTimeLeft < ONE_DAY) {
      // In a real application, you might redirect the user to re-authorize your app.
      throw new Error('Refresh token is expiring soon. Re-authorization required with Intuit.');
    }

    // Refresh the access token using the refresh token
    const tokenResponse = await this.oAuthClient.refreshUsingToken(refreshToken.value);
    const tokenData = tokenResponse.getJson();

    // Create new token objects
    const newAccessToken: Token = {
      value: tokenData.access_token,
      expiration: new Date(Date.now() + tokenData['expires_in'] * 1000),
    };

    const newRefreshToken: Token = {
      value: tokenData.refresh_token,
      expiration: new Date(Date.now() + tokenData['x_refresh_token_expires_in'] * 1000),
    };

    // Save the new refresh token and update the cached access token
    this.saveRefreshToken(newRefreshToken);
    this.saveAccessToken(newAccessToken);

    this.logger.log('Access token refreshed successfully.');

    return newAccessToken.value;
  }

  private saveRealmId(value: string) {
    this.save({ key: 'realmId', value });
  }

  private findRealmId(): string {
    return this.findOne('realmId');
  }

  private saveRefreshToken(value: Token) {
    this.save({ key: 'refreshToken', value });
  }

  private findRefreshToken(): Token {
    return this.findOne('refreshToken');
  }

  private saveAccessToken(value: Token) {
    this.save({ key: 'accessToken', value });
  }

  private findAccessToken(): Token {
    return this.findOne('accessToken');
  }

  private save({ key, value }: { key: string; value: string | Record<string, any> }) {
    const content = this.read();
    content[key] = value;
    this.write(content);
    return value;
  }

  private findOne(key: string) {
    const content = this.read();
    return content[key];
  }

  private read() {
    try {
      const buffer = fs.readFileSync(this.getFilename());
      const fileContent = buffer.toString();
      return JSON.parse(fileContent);
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
    } catch (e) {
      return {};
    }
  }

  private write(content: any) {
    try {
      const data = JSON.stringify(content, null, 2);
      fs.writeFileSync(this.getFilename(), data);
    } catch (e) {
      this.logger.error(`Cache error updating:`, e);
    }
  }

  private getFilename() {
    return `.${this.configService.get('ENV')}-cache.json`;
  }
}
