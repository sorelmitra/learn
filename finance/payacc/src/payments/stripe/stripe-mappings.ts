import { PaymentMethodComboInput, PaymentMethodType, PaymentStatusName } from '../dto/payments.dto';

export type StripePaymentMethodCombo = {
  payment_method?: string;
  payment_method_types?: string[];
};

export const getStripePaymentMethods = () =>
  new Map<PaymentMethodComboInput, StripePaymentMethodCombo>([
    [
      PaymentMethodComboInput.AchSuccess,
      {
        payment_method: 'pm_usBankAccount_success',
        payment_method_types: ['us_bank_account'],
      },
    ],
    [
      PaymentMethodComboInput.AchNotAuthorized,
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

export const getStripePaymentMethodTypeMappings = () =>
  new Map<string, PaymentMethodType>([['us_bank_account', PaymentMethodType.UsBankAccount]]);
