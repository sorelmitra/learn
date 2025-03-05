import { Injectable } from '@nestjs/common';
import { AccountingConnectionProcessorName } from './accounting-connection-processor';
import { AccountingConnectionFactory } from './accounting-connection.factory';

@Injectable()
export class AccountingConnectionService {
  constructor(private readonly factory: AccountingConnectionFactory) {}

  async connect(processorName: AccountingConnectionProcessorName) {
    const processor = this.factory.create(processorName);
    if (!processor) return;
    return processor.connect();
  }
}
