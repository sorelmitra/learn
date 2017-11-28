#!/bin/bash

CONSUL_NET_NAME="consul"
CONSUL_BASE_NAME="consul"

DO_START=0
DO_STOP=0
CONSUL_NODES="dev"

ALLOWED_CONSUL_NODES="dev dev-agent server agent"

while getopts 'dpn:' OPTION
do
    case $OPTION in
	1)  DO_START=1
		;;
	0)  DO_STOP=1
		;;
	n)  CONSUL_NODES="${OPTARG}"
		;;
	?)  usage
		exit 2
		;;
    esac
done

shift $(($OPTIND - 1))


consul_check_up() {
	CONSUL_INSTANCE_INFO=`docker ps | grep "${CONSUL_NAME}"`
	CONSUL_UP=0
	CONSUL_EXISTS=0
	if [[ -z "${CONSUL_INSTANCE_INFO}" ]]; then
		echo "Consul instance ${CONSUL_NAME} does not exist"
		return
	fi
	CONSUL_EXISTS=1
	CONSUL_INSTANCE_STATUS=`echo ${CONSUL_INSTANCE_INFO} | awk '{print $7;}'`
	if [[ "${CONSUL_INSTANCE_STATUS}" = "Up" ]]; then
		echo "Consul instance ${CONSUL_NAME} is up and running"
		docker ps | grep "${CONSUL_NAME}"
		CONSUL_UP=1
	fi
}

consul_remove() {
	echo "Consul instance ${CONSUL_NAME} exists, but is not up, removing it"
	CONSUL_REMOVED=0
	docker container rm ${CONSUL_NAME}
	CONSUL_INSTANCE_INFO=`docker ps | grep "${CONSUL_NAME}"`
	consul_check_up
	if [[ ${CONSUL_EXISTS} = 1 ]]; then
		echo "ERROR: consul instance ${CONSUL_NAME} was not removed"
		docker ps | grep "${CONSUL_NAME}"
		return
	fi
	CONSUL_REMOVED=1
	echo "Removed consul instance ${CONSUL_NAME}"
}

consul_start_dev() {
	echo "Starting development consul instance ${CONSUL_NAME}"
	docker run -d --name=${CONSUL_NAME} -p 8400:8400 -p8500:8500 -p 8600:8600/udp consul
	consul_check_up
	if [[ ${CONSUL_UP} = 0 ]]; then
		echo "Consul ${CONSUL_NAME} did not start"
	fi
}

consul_start_dev_agent() {
	echo "Starting development consul agent instance ${CONSUL_NAME}"
}

consul_start() {
	case ${CONSUL_NODE_TYPE} in
		"dev") consul_start_dev
			;;
		*) echo "Unknown node type ${CONSUL_NODE_TYPE}. Use one of ${ALLOWED_CONSUL_NODES}"
			CONSUL_UP=0
			;;
	esac
}


echo
echo ==========================================
echo "Setting up ${CONSUL_NET_NAME} docker network"

EXISTS=`docker network ls|grep ${CONSUL_NET_NAME}`
echo ${EXISTS} | grep -qiE "${CONSUL_NET_NAME}\s+bridge"
if [[ $? = 0 ]]; then
	echo "Docker network ${CONSUL_NET_NAME} exists"
else
	echo "Creating docker network ${CONSUL_NET_NAME}"
	docker network create \
		--driver=bridge \
		--subnet=10.1.1.0/24 \
		--ip-range=10.1.1.0/24 \
		--gateway=10.1.1.254 \
		${CONSUL_NET_NAME}
fi

docker network inspect ${CONSUL_NET_NAME}

echo
echo ==========================================
echo "Setting up consul nodes: ${CONSUL_NODES}"

i=0
for CONSUL_NODE_TYPE in "${CONSUL_NODES}"; do
	i=$((i+1))
	CONSUL_NAME="${CONSUL_BASE_NAME}-${CONSUL_NODE_TYPE}-${i}"
	consul_check_up
	if [[ ${CONSUL_UP} = 1 ]]; then continue; fi
	if [[ ${CONSUL_EXISTS} = 1 ]]; then consul_remove; fi
	if [[ ${CONSUL_REMOVED} = 0 ]]; then exit 1; fi
	consul_start
	if [[ ${CONSUL_UP} = 0 ]]; then exit 1; fi
done
