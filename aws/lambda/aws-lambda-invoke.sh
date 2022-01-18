#!/usr/bin/env bash

NAME=$1
shift

aws lambda invoke --cli-binary-format raw-in-base64-out --function-name ${NAME} --log-type Tail target/response.json | grep -E 'LogResult' | awk -F ':' '{print $2}' | awk -F '"' '{print $2}' | base64 --decode
