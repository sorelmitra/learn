import { Test, TestingModule } from '@nestjs/testing';
import { QuickBooksService } from './quick-books.service';

describe('QuickBooksService', () => {
  let service: QuickBooksService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [QuickBooksService],
    }).compile();

    service = module.get<QuickBooksService>(QuickBooksService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
