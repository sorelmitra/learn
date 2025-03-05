import { Logger, Module } from '@nestjs/common';
import { ConfigModule, ConfigService } from '@nestjs/config';
import { CacheModule } from '@nestjs/cache-manager';

@Module({
  imports: [
    ConfigModule.forRoot({
      envFilePath: `.${process.env.ENV}.env`,
    }),
    CacheModule.register(),
  ],
  providers: [Logger, ConfigService, CacheModule],
  exports: [Logger, ConfigService, CacheModule],
})
export class CommonModule {}
