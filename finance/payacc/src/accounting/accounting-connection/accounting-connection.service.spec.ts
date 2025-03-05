import { Test, TestingModule } from '@nestjs/testing';
import { AccountingConnectionService } from './accounting-connection.service';

describe('AccountingConnectionService', () => {
  let service: AccountingConnectionService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [AccountingConnectionService],
    }).compile();

    service = module.get<AccountingConnectionService>(AccountingConnectionService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
