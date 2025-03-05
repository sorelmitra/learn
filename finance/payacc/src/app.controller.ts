import { Body, Controller, Get, Param, Patch, Post, Query, Res } from '@nestjs/common';
import { AccountingConnectionProcessorName } from './accounting/accounting-connection/accounting-connection-processor';
import { AccountingConnectionService } from './accounting/accounting-connection/accounting-connection.service';
import { IntuitOAuthService as IntuitOAuthService } from './accounting/intuit/intuit-oauth/intuit-oauth.service';
import { AppService } from './app.service';
import { CreatePaymentInput, Payment, UpdatePaymentInput } from './payments/dto/payments.dto';
import { StripeEvent } from './payments/dto/stripe.dto';
import { PaymentsProcessorName } from './payments/processor/payments-processor';
import { StripeWebhookService } from './payments/stripe/webhook/stripe-webhook.service';

@Controller()
export class AppController {
  constructor(
    private readonly appService: AppService,
    private readonly accountingConnectionService: AccountingConnectionService,
    private readonly stripeWebhookService: StripeWebhookService,
    private readonly intuitOAuthService: IntuitOAuthService,
  ) {}

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

  @Get('/payments/:id')
  async getPayment(@Param('id') id: string): Promise<Payment> {
    return this.appService.getPayment(id);
  }

  @Patch('/payments/:id/confirm')
  async confirmPayment(@Param('id') id: string) {
    return this.appService.confirmPayment(id);
  }

  @Post('/webhook/stripe')
  async receiveStripeWebhook(@Body() event: StripeEvent) {
    return this.stripeWebhookService.receive(event);
  }

  @Get('/accounting/connection')
  async connectToAccountingProvider(
    @Query('processor') processorName: AccountingConnectionProcessorName,
    @Res() res,
  ) {
    const url = await this.accountingConnectionService.connect(processorName);
    res.status(302).redirect(url);
  }

  @Get('/oauth/callback/intuit')
  async receiveIntuitOAuthCallback(
    @Query('code') code: string,
    @Query('realmId') realmId?: string,
    @Query('state') state?: string,
  ) {
    return this.intuitOAuthService.performAuthorizationCodeGrant(code, realmId, state);
  }
}
