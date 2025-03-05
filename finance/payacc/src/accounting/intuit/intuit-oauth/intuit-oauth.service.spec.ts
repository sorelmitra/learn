import { Test, TestingModule } from '@nestjs/testing';
import { IntuitOauthService } from './intuit-oauth.service';

describe('IntuitOauthService', () => {
  let service: IntuitOauthService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [IntuitOauthService],
    }).compile();

    service = module.get<IntuitOauthService>(IntuitOauthService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
