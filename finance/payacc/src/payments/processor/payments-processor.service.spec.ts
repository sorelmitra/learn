import { Test, TestingModule } from '@nestjs/testing';
import { PaymentsProcessorService } from './payments-processor.service';

describe('PaymentsProcessorService', () => {
  let service: PaymentsProcessorService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [PaymentsProcessorService],
    }).compile();

    service = module.get<PaymentsProcessorService>(PaymentsProcessorService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
