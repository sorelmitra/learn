# API

Generalities
- See also `Stripe-Objects.png`.
https://docs.stripe.com/payments-api/tour

Payments
- Life-cycle: https://docs.stripe.com/payments/paymentintents/lifecycle
- Flow: https://docs.stripe.com/payments/accept-a-payment?ui=elements
- Overview: https://docs.stripe.com/payments/payment-intents
https://docs.stripe.com/api/payment_intents

Idempotent requests
https://docs.stripe.com/api/idempotent_requests

API Keys and Test Mode
https://docs.stripe.com/keys#obtain-api-keys

---
---
---
---
---
---
---
---
---
---

# Flows

## Off Session Payments

When a payment is made “on session”, the customer is present on your website or mobile application and is trying to pay you. If the payment fails, they can try again with a different payment method. If the payment requires an additional action (going through 3D Secure, being redirected to a partner like Klarna, etc.) they can go through those steps immediately to complete the payment.

By default, payments in the Stripe API are “on session”. You do not have to pass any specific parameter to indicate this, but if you want to be explicit you can pass the `off_session` parameter set as false (which is the default) when confirming the PaymentIntent.

When a payment is made “off session”, the customer is not present on your website or application to confirm the payment. Here are some examples of “off session” payments:

- Future automatic renewal payments associated with a subscription
- Charging a customer for a late fee or an incidental for a car rental
- MOTO payments where the customer is on the phone

References:

[1] https://support.stripe.com/questions/what-is-the-difference-between-on-session-and-off-session-and-why-is-it-important

## Form -- The Stripe Payment Form (or Element)

A description of the Stripe payment form can be found in [1].

Stripe attempts to control everything on it, the reason being that if you would have access to the entered data, that would put you in increased PCI compliance scope, which you don't want.  [2]

References:

[1]
https://stackoverflow.com/questions/57394620/stripe-createtoken-curious?utm_source=chatgpt.com

[2]
https://docs.stripe.com/payments/payment-element

## Form -- User Enters Invalid Card Info Flow

Summary of the various errors that the user can make, and answers to these questions for each:

1. When does Stripe (either the Form or the API) detect it?
2. What data does Stripe use in order to perform such detection?
3. Does Stripe save the Payment or Setup Intent along with its underlying Payment Method?

* **Error type**: (a) Badly-formatted card number
	- **Where/when it’s caught**: Instant, inside the browser in before any API call [1] [12] [13] [14]
	- **What data Stripe uses**: Front-end JS rules: BIN-length tables + Luhn check [2] [3]
	- **What gets written in Stripe**: No new `PaymentMethod`; any already-created `PaymentIntent/SetupIntent` remains in `requires_payment_method` (the API isn't even called)

* **Error type**: (b) Looks valid but issuer can’t find the account
	- **Where/when it’s caught**: During `confirmPaymentIntent` or `confirmSetupIntent` call, after the card network asks the issuer [4] [5] [6] [7]
	- **What data Stripe uses**: Real-time issuer response → Stripe decline code `invalid_account`/`incorrect_number` [8] [11]
	- **What gets written in Stripe**: `PaymentIntent/SetupIntent` persists; a transient `PaymentMethod` is returned in `last_payment_error.payment_method` but is **not** attached to the customer [9] [10]

* **Error type**: (c) Wrong CVC
	- **Where/when it’s caught**: At authorization; issuer runs the CVC check, as in (b)
	- **What data Stripe uses**: Issuer’s CVC-check result returned in `payment_method_details.card.checks.cvc_check`
	- **What gets written in Stripe**: Same as (b). Result can be `fail`, but the issuer may still approve; you decide with Radar rules whether to keep or retry

* **Error type**: (d) Wrong or expired date
	- **Where/when it’s caught**:
		* Bad format (e.g. month 13) → client-side, as in (a)
		* Expired/mismatching date → issuer during authorization → decline code `expired_card`, as in (b)
	- **What data Stripe uses**: Client JS length/range checks + issuer auth response
	- **What gets written in Stripe**: For issuer declines, same persistence pattern as (b)

* **Error type**: (e) Billing-detail mismatch (ZIP, street, name)
	- **Where/when it’s caught**: During authorization; AVS/CVC checks run by issuer
	- **What data Stripe uses**: AVS/CVC signals returned in `card.checks.address_postal_code_check`, `address_line1_check`, etc.
	- **What gets written in Stripe**: `PaymentMethod` with check results is created but probably not attached. `PaymentIntent` remains regardless [4]

Further notes:

* (I) Once stored, a Payment Method [cannot be updated](https://docs.stripe.com/api/payment_methods/update) in it's essential details, such as card number or CVC.

References:

[1] https://docs.stripe.com/js/element/input_validation?type=cardNumberElement

[2] https://stripe.com/en-ro/resources/more/how-to-use-the-luhn-algorithm-a-guide-in-applications-for-businesses

[3] https://docs.stripe.com/issuing/customize-your-program

[4] https://docs.stripe.com/disputes/prevention/verification

[5] https://docs.stripe.com/api/payment_intents/confirm

[6] https://docs.stripe.com/api/setup_intents/confirm

[7] https://support.stripe.com/questions/confirm-success-of-a-setupintent

[8] https://docs.stripe.com/testing#declined-payments

[9] https://docs.stripe.com/error-codes#payment-intent-payment-attempt-failed

[10] https://docs.stripe.com/api/payment_intents/object#payment_intent_object-last_payment_error

[11] https://docs.stripe.com/declines/codes

[12] https://docs.stripe.com/payments/accept-a-payment-charges

[13] https://docs.stripe.com/payments/checkout/subscriptions/update-payment-details

[14] https://stackoverflow.com/questions/65862556/stripe-elements-how-can-i-validate-a-card-element-is-not-blank-client-side

## Form -- ACH Data Collection Flow

* **Category:** Bank routing details
	- **Field collected in the form:** _Routing number_
	- **Required?** _Yes_ (for US bank accounts)
	- **Notes:** Must be a valid 9-digit ABA number. [1] [2]

* **Category:** Bank routing details
	- **Field collected in the form:** _Account number_
	- **Required?** _Yes_
	- **Notes:** Stripe tokenises the full account number; only the last 4 are stored long-term. [3]

* **Category:** Account identification
	- **Field collected in the form:** _Account-holder name_
	- **Required?** _Yes_
	- **Notes:** Nacha requires the customer’s name to appear on the mandate; Stripe enforces this by making `billing_details.name` mandatory when the payment method is created. [5]

* **Category:** Account identification
	- **Field collected in the form:** _Account type_ (checking / saving)
	- **Required?** No – defaults to *checking* if omitted
	- **Notes:** Passed as `us_bank_account.account_type`; useful for reconciliation but not mandatory. [6]

* **Category:** Account identification
	- **Field collected in the form:** _Account-holder type_ (individual / company)
	- **Required?** _Yes_, when attaching bank account to a Customer
	- **Notes:** Determines the SEC code (WEB vs CCD). If you leave it blank Stripe assumes *individual*. [6] [7]

* **Category:** Customer contact
	- **Field collected in the form:** _Email address_
	- **Required?** ???
	- **Notes:** The Payment Element lets you hide, auto-collect, or make them conditional with the `billingDetails` flags. [9]

* **Category:** Customer contact
	- **Field collected in the form:** _Phone, billing address_
	- **Required?** ???
	- **Notes:** The Payment Element lets you hide, auto-collect, or make them conditional with the `billingDetails` flags. [9]

* **Category:** Mandate authorization
	- **Field collected in the form:** _One-click acceptance_ (checkbox / “Pay” button)
	- **Required?** _Yes_
	- **Notes:** Stripe records acceptance status and a few other fields. [10]

* **Category:** Verification (fallback)
	- **Field collected in the form:** _Micro-deposit amounts_ (customer verifies their account by entering some amounts they see on their statement)
	- **Required?** No
	- **Notes:** Stripe switches to this flow only if instant verification is skipped or fails. [14] [15]

[1] https://docs.stripe.com/api/customer_bank_accounts/create

[2] https://docs.stripe.com/financial-connections/ach-direct-debit-payments

[3] https://stackoverflow.com/questions/47323685/how-can-i-get-bank-account-number-from-source-in-stripe-api

[5] https://docs.stripe.com/billing/subscriptions/ach-debit?platform=web&payment-ui=elements#collect-payment-details

[6] https://docs.stripe.com/api/customer_bank_accounts/create#customer_create_bank_account-source-account_holder_type

[7] https://docs.stripe.com/treasury/moving-money/standard-entry-class

[9] https://docs.stripe.com/payments/payment-element/control-billing-details-collection

[10] https://docs.stripe.com/api/mandates/object

[14] https://stripe.com/blog/accept-ach-payments

[15] https://docs.stripe.com/financial-connections/use-cases

## Payment Link Flow

As we can see from the flow below, the *account ID* and *code* (the one starting with `test_YWN` below) are up-front, and calling to https://invoicedata.stripe.com/hosted_invoice_page/ with them gets all the info we need in order to trigger payment in Stripe.  No other encryption or obfuscation is employed.  As
 [1] points out, any such obfuscation is, in fact, vanity, not security.

[1] https://stackoverflow.com/a/68844235/6239668

The payment link flow:

1) Get the payment link from the merchant, e.g.
https://invoice.stripe.com/i/acct_1PahM52MOMlJvmj7/test_YWNjdF8xUGFoTTUyTU9NbEp2bWo3LF9RYmRZY0l3ellkS1ltZk1BMjZMcTFVT2FReXM4QmpGLDExMzU3MzQ5Ng02009Ki0kovy?s=db

2) Open it into a browser

3) Browser calls to `hosted_invoice_page`, providing the
- account number, e.g. `acct_1PahM52MOMlJvmj7`
- code it received, e.g. `test_YWNjdF8xUGFoTTUyTU9NbEp2bWo3LF9RYmRZY0l3ellkS1ltZk1BMjZMcTFVT2FReXM4QmpGLDExMzU3MzQ5Ng02009Ki0kovy`

Full request made, e.g.
https://invoicedata.stripe.com/hosted_invoice_page/acct_1PahM52MOMlJvmj7/test_YWNjdF8xUGFoTTUyTU9NbEp2bWo3LF9RYmRZY0l3ellkS1ltZk1BMjZMcTFVT2FReXM4QmpGLDExMzU3MzQ5Ng02009Ki0kovy?creditNoteRecoverySlug=

4) Stripe's hosted invoice page responds with all the data necessary in order to initiate the payment, including:
- invoice ID, e.g. `in_1PkQH22MOMlJvmj7FhaUgWCq`
- payee's name, e.g. `John Test`
- billing address, etc.

## Payouts Flow

Detailed payout data (such as transaction ID, customer, etc.) is not available at request from Stripe.  Instead, you have to run a report.  Options are:

1) Payout reconciliation report: payouts received in your bank account, including transactions they relate to.

- API report type: `payout_reconciliation.itemized.5`
- Docs: https://docs.stripe.com/reports/report-types/payout-reconciliation

2) Connected account itemized single payout reconciliation.  By default, the API returns report data for your platform account activity. To view data for your connected accounts, use the Connect-specific report types listed below.

- API report type: `connected_account_payout_reconciliation.by_id.itemized.4`
- Docs: https://docs.stripe.com/reports/report-types/connect#schema-connected-account-payout-reconciliation-by-id-itemized-4

Documentation on running these reports
https://docs.stripe.com/reports/api -> "Recommended integration pattern for automated reporting"

---
---
---
---
---
---
---
---
---
---

# Testing

To choose the correct account for testing:
Stripe
	-> Enable Test mode
	-> Dashboard -> Products -> Connect -> Connected Accounts
	-> Choose any tab, e.g. "All", "Restricted", "Complete" (this one has payments)
	-> Choose your account, e.g. "John Test" -> Click on it

Payment testing
https://docs.stripe.com/testing?testing-method=card-numbers

Payouts testing
- Mark the ACH Payment as succeeded
  `POST /v1/test_helpers/payment_intents/{PAYMENT_INTENT_ID}/succeed`
- Create a test payout
  `POST /v1/payouts`
- To match payouts to payment intents, you can do one of:
	* Run a report, see above.  This is the correct way to do it in a real life app.
	* Manually find it (for debugging purposes):
		-> If using atest payout, make sure it matches the PI amount
		-> List all recent balance transactions, `/v1/balance_transactions`.
		-> Find the Payment Intents (`type: payment`) **before** the payout, that match the payout amount.

Example applications
https://docs.stripe.com/terminal/example-applications?terminal-sdk-platform=ios

Terminal integration testing
https://docs.stripe.com/terminal/references/testing?terminal-sdk-platform=ios

---
---
---
---
---
---
---
---
---
---

# Useful tips and commands

## Update metadata on an object

curl https://api.stripe.com/v1/<OBJECT_TYPE>/<OBJECT_ID> \
  -u sk_test_1234567890: \
  -X POST \
  -d "metadata[agentEntered]=false" \
  -d "metadata[cardBrand]=STRIPE TEST BANK" \
  -d "metadata[city]=Roseville" \
  -d "metadata[email]=john@doe.com" \
  -d "metadata[firstName]=Savely" \
  -d "metadata[lastName]=Badpayment" \
  -d "metadata[line1]=4 Medical Plaza Drive" \
  -d "metadata[paymentMethodType]=us_bank_account" \
  -d "metadata[postalCode]=95661" \
  -d "metadata[salesRepEmail]=ward@bar.com" \
  -d "metadata[salesRepFirstName]=LO" \
  -d "metadata[salesRepLastName]=Ward" \
  -d "metadata[state]=CA" \
  -d "metadata[transactionId]=250818G0022699"
