BUNDLE_NAME := my-lambda
TARGET_DIR := target
DIST_CODE_DIR := ${TARGET_DIR}/${BUNDLE_NAME}
BUNDLE_DIR_NAME := bundle
BUNDLE_DIR := ${DIST_CODE_DIR}/../${BUNDLE_DIR_NAME}
# Version key/value should be on his own line
PACKAGE_VERSION=0.1.0
TARGET=${BUNDLE_NAME}-${PACKAGE_VERSION}
BUNDLED_TGZ=contactme-${PACKAGE_VERSION}.tgz
TARGET_TGZ=${TARGET}.tgz
TARGET_ZIP=${TARGET}.zip

all: bundle deploy run

compile:
	mkdir -p ${DIST_CODE_DIR}
	npx babel --verbose --extensions '.ts' --out-dir ${DIST_CODE_DIR} --ignore **/__tests__ src

prodprep:
	rm -rf ${DIST_CODE_DIR}/node_modules ${DIST_CODE_DIR}/package-lock.json
	cp package.json ${DIST_CODE_DIR}
	cd ${DIST_CODE_DIR} && npm install --production

zip:
	mkdir -p ${BUNDLE_DIR}
	cd ${DIST_CODE_DIR} && zip -r -q ../${BUNDLE_DIR_NAME}/${TARGET_ZIP} .

bundle: compile prodprep zip

deploy:
	AWS_PAGER="" aws lambda update-function-code --function-name ${BUNDLE_NAME} --zip-file fileb://${BUNDLE_DIR}/${TARGET_ZIP}

run:
	./aws-lambda-invoke.sh ${BUNDLE_NAME}

clean:
	rm -rfv ${TARGET_DIR}/

.PHONY: compile prodprep zip deploy
