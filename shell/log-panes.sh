#!/usr/bin/env bash
# thanks to http://unix.stackexchange.com/a/358209/50703 for the help on this one

# not sure why we use the -g option
# default-shell is here to prevent tmux from inheriting say the fish shell, which will break the && exit
export command; command="tmux new -s logs \; set-option -g default-shell '$(which bash)'"

# Kube namespace, pod and container name
# For testing purposes, these pods usually exist
namespace=kube-system
name=kube-flannel
container_name=kube-flannel

if [[ ! -z "$1" ]]; then
	namespace=$1
	shift
fi

if [[ ! -z "$1" ]]; then
	name=$1
	container_name=${name}
	shift
fi

if [[ ! -z "$1" ]]; then
	container_name=$1
	shift
fi

pods=(`kubectl get pods --namespace ${namespace} | grep ${name} | awk '{print $1}'`)

for pod in ${pods[@]}; do
	log_command="kubectl logs --namespace ${namespace} ${pod} -c ${container_name} -f"
	command="$command \; send-keys '${log_command}' C-m \; split-window ${split_arg} \; select-layout tiled"
done

#if [[ $(( ${#pods[@]} % 2 )) -eq 0 ]]; then
#	command="$command \; kill-pane"
#fi

eval "$command"
