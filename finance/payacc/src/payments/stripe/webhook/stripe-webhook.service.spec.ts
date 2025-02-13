import { Test, TestingModule } from '@nestjs/testing';
import { StripeWebhookService } from './stripe-webhook.service';
import { Logger } from '@nestjs/common';
import { QueueService } from 'src/common/queue/queue.service';

describe('StripeWebhookService', () => {
  let service: StripeWebhookService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [Logger, QueueService, StripeWebhookService],
    }).compile();

    service = module.get<StripeWebhookService>(StripeWebhookService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
