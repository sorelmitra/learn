LAMBDA_NAME=$1
shift
PAYLOAD=$1
shift

aws lambda invoke --cli-binary-format raw-in-base64-out --function-name ${LAMBDA_NAME} --payload ${PAYLOAD} --log-type Tail response.json | grep -Ei 'LogResult' | awk -F ':' '{print $2}' | awk -F '"' '{print $2}' | base64 --decode
