import { Test, TestingModule } from '@nestjs/testing';
import { IntuitOAuthService } from './intuit-oauth.service';

describe('IntuitOauthService', () => {
  let service: IntuitOAuthService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [IntuitOAuthService],
    }).compile();

    service = module.get<IntuitOAuthService>(IntuitOAuthService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
