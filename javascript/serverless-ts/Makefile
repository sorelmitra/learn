
lambdas_deploy = \
	repo.deploy \
	repo_observer.deploy \
	order_observer.deploy \
	receipt_observer.deploy \
	order_email.deploy \
	receipt_store.deploy \

lambdas_unittest = $(lambdas_deploy:deploy=unittest)
lambdas_autotest = $(lambdas_deploy:deploy=autotest)

repo.dir := lambda/pizzem-repo
repo_observer.dir := lambda/pizzem-repo-observer
order_observer.dir := lambda/pizzem-order-observer
receipt_observer.dir := lambda/pizzem-receipt-observer
order_email.dir := lambda/pizzem-order-email
receipt_store.dir := lambda/pizzem-receipt-store

repo.autotest.dir := test/sys/repo
repo_observer.autotest.dir := test/sys/repo-observer
order_observer.autotest.dir := test/sys/order-observer
receipt_observer.autotest.dir := test/sys/receipt-observer
order_email.autotest.dir := test/sys/order-email
receipt_store.autotest.dir := test/sys/receipt-store

deploy: $(lambdas_deploy)

unittest: $(lambdas_unittest)

autotest: $(lambdas_autotest)

%.deploy:
	$(eval target_name := $(@:.deploy=))
	cd $($(target_name).dir) && make

%.unittest:
	$(eval target_name := $(@:.unittest=))
	cd $($(target_name).dir) && npm test

%.autotest:
	$(eval target_name := $(@:.autotest=))
	pytest $($(target_name).autotest.dir)

