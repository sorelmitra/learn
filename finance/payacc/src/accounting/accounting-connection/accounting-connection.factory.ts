import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import {
  AccountingConnectionProcessor,
  AccountingConnectionProcessorName,
} from './accounting-connection-processor';
import { IntuitConnectionService } from '../intuit/intuit-connection/intuit-connection.service';

@Injectable()
export class AccountingConnectionFactory {
  constructor(private readonly intuitConnectionService: IntuitConnectionService) {}

  private processorsMap = new Map<AccountingConnectionProcessorName, AccountingConnectionProcessor>(
    [[AccountingConnectionProcessorName.INTUIT, this.intuitConnectionService]],
  );

  create(proc?: AccountingConnectionProcessorName): AccountingConnectionProcessor {
    const processor = this.processorsMap.get(proc ?? AccountingConnectionProcessorName.INTUIT);
    if (!processor) {
      throw new HttpException(
        `Unknown accounting connection processor ${proc}`,
        HttpStatus.BAD_REQUEST,
      );
    }
    return processor;
  }
}
