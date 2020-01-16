# Introduction

This documents my attempts at installing a kubernetes cluster locally using minikube.

# Resources

[1] https://kubernetes.io/docs/setup/learning-environment/minikube/

# Installation

## Linux

1. Install Homebrew

		sudo apt install linuxbrew-wrapper

2. Install Minikube

		brew install minikube
	
	On my Ubuntu 18.04 this also updated brew to Homebrew. Then I had to add `/home/linuxbrew/.linuxbrew/bin` to `PATH`. Then I had to run the above command *again*.

