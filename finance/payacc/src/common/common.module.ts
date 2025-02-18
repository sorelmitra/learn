import { Logger, Module } from '@nestjs/common';
import { ConfigModule, ConfigService } from '@nestjs/config';

@Module({
  imports: [
    ConfigModule.forRoot({
      envFilePath: `.${process.env.ENV}.env`,
    }),
  ],
  providers: [Logger, ConfigService],
  exports: [Logger, ConfigService],
})
export class CommonModule {}
