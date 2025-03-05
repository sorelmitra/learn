import * as fs from 'fs';
import { HttpException, HttpStatus, Inject, Injectable, Logger } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import * as OAuthClient from 'intuit-oauth';
import { Cache } from 'cache-manager';
import { CACHE_MANAGER } from '@nestjs/cache-manager';

type Token = {
  value: string;
  expiration: Date;
}

@Injectable()
export class IntuitOauthService {
  constructor(
    private readonly logger: Logger,
    private readonly configService: ConfigService,
    private readonly oAuthClient: OAuthClient,
    @Inject(CACHE_MANAGER) private cache: Cache,
  ) {}

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
    const json = resp.getJson();

    const refreshToken: Token = {
      value: json['refresh_token'],
      expiration: new Date(Date.now() + new Number(json['x_refresh_token_expires_in']).valueOf() * 1000),
    };
    const accessToken: Token = {
      value: json['access_token'],
      expiration: new Date(Date.now() + new Number(json['expires_in']).valueOf() * 1000),
    };
    this.saveRefreshToken(refreshToken);
    this.cache.set('QB_ACCESS_TOKEN', accessToken);

    return JSON.stringify({
      message: `Successfully connected to Intuit, access token expires on ${accessToken.expiration.toISOString()} minutes, refresh token expires on ${refreshToken.expiration.toISOString()}`,
    });
  }

  private saveRefreshToken(value: Token) {
    this.save({ key: 'refreshToken', value });
  }

  private findRefreshToken() {
    this.findOne('refreshToken');
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

  private getFilename(): any {
    return `.${this.configService.get('ENV')}-cache.json`;
  }
}
