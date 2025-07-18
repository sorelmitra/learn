#!/bin/zsh

# Starting a session
# "devsh" stands for "Development Shell" -- a shell for running apps in development
tmux new-session -d -s devsh-acme -n stuff

# Starting a few services, each in its own window
# Type <Prefix> : kill-session <Enter> in order to kill them all

tmux new-window -n Acme-Backend -c ~/1data/s/sorelmitra/learn/javascript/single-spa/acme-backend
tmux send-keys "yarn run build" Enter
tmux send-keys "yarn run watch" Enter

tmux new-window -n Acme-Root -c ~/1data/s/sorelmitra/learn/javascript/single-spa/acme-root
tmux send-keys "yarn run start" Enter

tmux new-window -n Ancient-Lag -c ~/1data/s/sorelmitra/learn/javascript/single-spa/ancient-lag
tmux send-keys "yarn run start" Enter

# Attach to the new session
tmux attach
