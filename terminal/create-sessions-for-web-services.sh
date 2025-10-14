#!/bin/zsh

DIR=$0:A:h
VERBOSE=1
source ${DIR}/../../../tools/lib.sh

do_start_service_p=0
do_start_service_a=0

usage() {
	echo
	echo "This script starts a TMUX development session with a few services and tools for a typical web project."
	echo
	echo "Arguments:"
	echo "	-p: Start Service 'P' instead of Service 'C'; default: false"
	echo "	-a: Start Service 'A'; default: false"
	echo
	if [ "$1" != "" ]; then
		echo "$1!"
		echo
	fi	
}

while getopts 'pa' OPTION; do
	case $OPTION in
		p) do_start_service_p=1 ;;
		a) do_start_service_a=1 ;;
		*) usage; exit ;;
	esac
done

# Starting a session
tmux new-session -d -s web-services -n stuff

# Starting a few services, each in its own window
# Type <Prefix> : kill-session <Enter> in order to kill them all

if [[ ${do_start_service_p} -eq 1 ]]; then
	tmux new-window -n Service P 
	tmux send-keys "cd ~/1data/s/my-projects/service-p" Enter
	tmux send-keys "git branch && git pull; npm install; STAGE=dev npm run start:api" Enter
else
	tmux new-window -n Service C 
	tmux send-keys "cd ~/1data/s/my-projects/service-c" Enter
	tmux send-keys "git branch && git pull; npm install; STAGE=dev npm run start:api" Enter
fi

if [[ ${do_start_service_a} -eq 1 ]]; then
	tmux new-window -n Service A 
	tmux send-keys "cd ~/1data/s/my-projects/service-a" Enter
	tmux send-keys "git branch && git pull; npm install; STAGE=dev npm run start:api" Enter
fi

tmux new-window -n Service B 
tmux send-keys "cd ~/1data/s/my-projects/service-b" Enter
tmux send-keys "git branch && git pull; npm install; STAGE=dev npm run start:api" Enter

tmux new-window -n Fwd-Stripe
# This one assumes that on localhost:8731 there's a service
# capable of receiving Stripe Webhooks
tmux send-keys "STRIPE_KEY=<Stripe Test-Key> && stripe listen --api-key \${STRIPE_KEY} --forward-to http://localhost:8731/webhook/stripe/local" Enter

tmux new-window -n Fwd-SNS 
tmux send-keys "cd ~/1data/s/sorelmitra/learn/aws/sns" Enter
tmux send-keys "AWS_PROFILE=test-profile REGION=us-west-2 QUEUE_URLS=http://localhost:9334/queue/queue-local.fifo,http://localhost:9344/queue/queue-local.fifo node local-sns.js" Enter

tmux new-window -n Tools

tmux new-window -n "(nohist)"
tmux send-keys "source ~/1data/s/sorelmitra/learn/terminal/incognito.sh" Enter

# Attach to the new session
tmux attach
