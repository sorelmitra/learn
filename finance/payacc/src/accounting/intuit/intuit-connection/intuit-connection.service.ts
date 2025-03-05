import { Injectable } from '@nestjs/common';
import { AccountingConnectionProcessor } from 'src/accounting/accounting-connection/accounting-connection-processor';
import * as OAuthClient from 'intuit-oauth';

@Injectable()
export class IntuitConnectionService extends AccountingConnectionProcessor {
  constructor(private readonly oAuthClient: OAuthClient) {
    super();
  }

  async connect(): Promise<void> {
    return await this.oAuthClient.authorizeUri({
      scope: ['com.intuit.quickbooks.accounting'],
      state: 'testState', // Optional, but useful for CSRF protection
    });
  }
}
