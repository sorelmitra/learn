import { IsBoolean, IsNumber, IsObject, IsOptional, IsString } from 'class-validator';

export class StripeEvent {
  @IsString()
  id: string;

  @IsString()
  type: string;

  @IsString()
  object: string;

  @IsString()
  @IsOptional()
  api_version?: string;

  @IsObject()
  data: {
    object: object;
    previous_attributes?: object;
  };

  @IsObject()
  @IsOptional()
  request?: {
    id?: string;
    idempotency_key?: string;
  };

  @IsString()
  @IsOptional()
  account?: string;

  @IsNumber()
  created: number;

  @IsBoolean()
  livemode: boolean;

  @IsNumber()
  pending_webhooks: number;
}
