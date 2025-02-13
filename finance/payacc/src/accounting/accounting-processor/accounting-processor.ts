export enum AccountingProcessorName {
  QUICK_BOOKS = 'QuickBooks',
}

export interface AccountingProcessor {
}

export type AccountingProcessorAndId = {
  processor: AccountingProcessor;
  processorId: string;
};
