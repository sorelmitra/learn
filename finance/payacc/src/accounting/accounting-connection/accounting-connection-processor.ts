export enum AccountingConnectionProcessorName {
  INTUIT = 'Intuit',
}

export abstract class AccountingConnectionProcessor {
  abstract connect(): Promise<void>;
}
