DIR_LAMBDA_REPO := lambda/pizzem-repo
DIR_LAMBDA_REPO_OBSERVER := lambda/pizzem-repo-observer

DIR_AUTOTEST_LAMBDA_REPO := test/sys/repo
DIR_AUTOTEST_LAMBDA_REPO_OBSERVER := test/sys/repo-observer

lambdas: lambda_repo lambda_repo_observer

lambda_repo:
	cd $(DIR_LAMBDA_REPO) && make

lambda_repo_observer:
	cd $(DIR_LAMBDA_REPO_OBSERVER) && make

autotest: autotest_lambda_repo autotest_lambda_repo_observer

autotest_lambda_repo:
	pytest $(DIR_AUTOTEST_LAMBDA_REPO)

autotest_lambda_repo_observer:
	pytest $(DIR_AUTOTEST_LAMBDA_REPO_OBSERVER)

unittest: unittest_lambda_repo unittest_lambda_repo_observer

unittest_lambda_repo:
	cd $(DIR_LAMBDA_REPO) && npm test

unittest_lambda_repo_observer:
	cd $(DIR_LAMBDA_REPO_OBSERVER) && npm test

.PHONY: test autotest_lambda_repo autotest_lambda_repo_observer lambdas lambda_repo lambda_repo_observer
