import { Test, TestingModule } from '@nestjs/testing';
import { IntuitConnectionService } from './intuit-connection.service';

describe('IntuitConnectionService', () => {
  let service: IntuitConnectionService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [IntuitConnectionService],
    }).compile();

    service = module.get<IntuitConnectionService>(IntuitConnectionService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
