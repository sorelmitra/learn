BUNDLE_NAME := pizzemQueryPizza
TARGET_DIR := ../../target/pizzem
DIST_CODE_DIR := ${TARGET_DIR}/dist
BUNDLE_DIR_NAME := bundle
BUNDLE_DIR := ${DIST_CODE_DIR}/../${BUNDLE_DIR_NAME}
# Version key/value should be on his own line
PACKAGE_VERSION=0.1.0
TARGET=${BUNDLE_NAME}-${PACKAGE_VERSION}
BUNDLED_TGZ=contactme-${PACKAGE_VERSION}.tgz
TARGET_TGZ=${TARGET}.tgz
TARGET_ZIP=${TARGET}.zip

all: deploy

compile:
	mkdir -p ${DIST_CODE_DIR}
	npx babel --verbose --extensions '.ts' --out-dir ${DIST_CODE_DIR} src

prodprep:
	rm -rf ${DIST_CODE_DIR}/node_modules
	cp -r node_modules ${DIST_CODE_DIR}
	cp package.json ${DIST_CODE_DIR}
	cd ${DIST_CODE_DIR} && npm prune --production

zip:
	mkdir -p ${BUNDLE_DIR}
	cd ${DIST_CODE_DIR} && zip -r ../${BUNDLE_DIR_NAME}/${TARGET_ZIP} .

bundle: compile prodprep zip

deploy: bundle
	aws lambda update-function-code --function-name ${BUNDLE_NAME} --zip-file fileb://${BUNDLE_DIR}/${TARGET_ZIP}

clean:
	rm -rfv ${TARGET_DIR}/

.PHONY: compile prodprep zip deploy
