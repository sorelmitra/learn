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

all: compile deploy

compile:
	npx tsc
	cp -r node_modules ${DIST_CODE_DIR}

zip:
	mkdir -p ${BUNDLE_DIR}
	cd ${DIST_CODE_DIR} && zip -r ../${BUNDLE_DIR_NAME}/${TARGET_ZIP} .

deploy: zip
	aws lambda update-function-code --function-name ${BUNDLE_NAME} --zip-file fileb://${BUNDLE_DIR}/${TARGET_ZIP}

clean:
	rm -rfv ${TARGET_DIR}/

.PHONY: compile zip deploy
