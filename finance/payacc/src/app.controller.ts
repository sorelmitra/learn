import { Body, Controller, Get, Param, Patch, Post, Query } from '@nestjs/common';
import { AppService } from './app.service';
import { CreatePaymentInput, Payment, UpdatePaymentInput } from './payments/dto/payments.dto';
import { PaymentsProcessorName } from './payments/processor/payments-processor';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get('/health')
  getHello(): string {
    return this.appService.getHello();
  }

  @Post('/payments')
  async createPayment(
    @Query('proc') proc: PaymentsProcessorName,
    @Body() input: CreatePaymentInput,
  ): Promise<Payment> {
    return this.appService.createPayment({ input, proc });
  }

  @Patch('/payments/:id')
  async updatePayment(
    @Param('id') id: string,
    @Body() input: UpdatePaymentInput,
  ): Promise<Payment> {
    return this.appService.updatePayment({ id, input });
  }

  @Patch('/payments/:id/confirm')
  async confirmPayment(@Param('id') id: string) {
    return this.appService.confirmPayment(id);
  }
}
