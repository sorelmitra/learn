DIR_LAMBDA_REPO := lambda/pizzem-repo
DIR_LAMBDA_REPO_OBSERVER := lambda/pizzem-repo-observer
DIR_LAMBDA_ORDER_OBSERVER := lambda/pizzem-order-observer
DIR_LAMBDA_RECEIPT_OBSERVER := lambda/pizzem-receipt-observer

DIR_AUTOTEST_LAMBDA_REPO := test/sys/repo
DIR_AUTOTEST_LAMBDA_REPO_OBSERVER := test/sys/repo-observer
DIR_AUTOTEST_LAMBDA_ORDER_OBSERVER := test/sys/order-observer
DIR_AUTOTEST_LAMBDA_RECEIPT_OBSERVER := test/sys/receipt-observer

lambdas: lambda_repo lambda_repo_observer lambda_order_observer lambda_receipt_observer

lambda_repo:
	cd $(DIR_LAMBDA_REPO) && make

lambda_repo_observer:
	cd $(DIR_LAMBDA_REPO_OBSERVER) && make

lambda_order_observer:
	cd $(DIR_LAMBDA_ORDER_OBSERVER) && make

lambda_receipt_observer:
	cd $(DIR_LAMBDA_RECEIPT_OBSERVER) && make

autotest: autotest_lambda_repo autotest_lambda_repo_observer autotest_lambda_order_observer autotest_lambda_receipt_observer

autotest_lambda_repo:
	pytest $(DIR_AUTOTEST_LAMBDA_REPO)

autotest_lambda_repo_observer:
	pytest $(DIR_AUTOTEST_LAMBDA_REPO_OBSERVER)

autotest_lambda_order_observer:
	pytest $(DIR_AUTOTEST_LAMBDA_ORDER_OBSERVER)

autotest_lambda_receipt_observer:
	pytest $(DIR_AUTOTEST_LAMBDA_RECEIPT_OBSERVER)

unittest: unittest_lambda_repo unittest_lambda_repo_observer unittest_lambda_order_observer unittest_lambda_receipt_observer

unittest_lambda_repo:
	cd $(DIR_LAMBDA_REPO) && npm test

unittest_lambda_repo_observer:
	cd $(DIR_LAMBDA_REPO_OBSERVER) && npm test

unittest_lambda_order_observer:
	cd $(DIR_LAMBDA_ORDER_OBSERVER) && npm test

unittest_lambda_receipt_observer:
	cd $(DIR_LAMBDA_RECEIPT_OBSERVER) && npm test

.PHONY: test autotest_lambda_repo autotest_lambda_repo_observer lambdas lambda_repo lambda_repo_observer
