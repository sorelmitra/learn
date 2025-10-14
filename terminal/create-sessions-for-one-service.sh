#!/bin/zsh

# Starting a session
# "devsh" stands for "Development Shell" -- a shell for running apps in development
tmux new-session -d -s devsh-one-service -n stuff

# Creating a window for the service
tmux new-window -n Service

tmux new-window -n Tools

tmux new-window -n "(nohist)"
tmux send-keys "source ~/1data/s/sorelmitra/learn/terminal/incognito.sh" Enter

# Attach to the new session
tmux attach
