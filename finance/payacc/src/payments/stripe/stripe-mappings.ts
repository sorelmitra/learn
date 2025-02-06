import { PaymentStatusName } from '../dto/payments.dto';
import { PaymentMethodName } from '../processor/payments-processor';

export type PaymentMethodMapping = {
  payment_method?: string;
  payment_method_types?: string[];
};

export const getStripePaymentMethods = () =>
  new Map<PaymentMethodName, PaymentMethodMapping>([
    [
      PaymentMethodName.Success,
      {
        payment_method: 'pm_usBankAccount_success',
        payment_method_types: ['us_bank_account'],
      },
    ],
    [
      PaymentMethodName.AchNotAuthorized,
      {
        payment_method: 'pm_usBankAccount_debitNotAuthorized',
        payment_method_types: ['us_bank_account'],
      },
    ],
  ]);

export const getStripeStatusMappings = () =>
  new Map<string, PaymentStatusName>([
    ['requires_payment_method', PaymentStatusName.Open],
    ['requires_confirmation', PaymentStatusName.Open],
    ['processing', PaymentStatusName.Pending],
    ['failed', PaymentStatusName.Failed],
    ['succeeded', PaymentStatusName.Succeeded],
  ]);
