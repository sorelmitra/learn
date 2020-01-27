# Introduction

Trying to learn about Kubernetes.

A Kube playground: https://labs.play-with-k8s.com/. Not that useful, as it's not safe to use real passwords with it, so you can't download any image from a private repo.



# Resources

[1] A series of [1a-e], explaining Kubernetes Concepts

[1a] https://blog.giantswarm.io/understanding-basic-kubernetes-concepts-i-introduction-to-pods-labels-replicas/

[1b] https://blog.giantswarm.io/understanding-basic-kubernetes-concepts-using-deployments-manage-services-declaratively/

[1c] https://blog.giantswarm.io/basic-kubernetes-concepts-iii-services-give-abstraction/

[1d] https://blog.giantswarm.io/understanding-basic-kubernetes-concepts-iv-secrets-and-configmaps/

[1e] https://blog.giantswarm.io/understanding-basic-kubernetes-concepts-v-daemon-sets-and-jobs/

[2] http://kubernetesbyexample.com - a good companion to [1]

[3] https://www.oreilly.com/library/view/managing-kubernetes/9781492033905/ch04.html

[4] https://kubernetes.io/docs/reference/command-line-tools-reference/kubelet/

[5] https://kubernetes.io/blog/2016/04/using-deployment-objects-with/

[6] https://kubernetes.io/docs/tasks/configure-pod-container/configure-liveness-readiness-startup-probes/

[7] https://kubernetes.io/docs/tasks/configure-pod-container/pull-image-private-registry/

[8] https://managedkube.com/kubernetes/k8sbot/troubleshooting/imagepullbackoff/2019/02/23/imagepullbackoff.html



# Lecture Notes

## Kubernetes by Example [2]

This is oriented towards demonstrating Kubernetes concepts with examples. It also explains a bit those concepts, but it's most value comes from the examples. It is a good companion to [1].

## Kubernetes Documentation

*API Server*: The API server is the gateway to the Kubernetes cluster. It is the central touch point that is accessed by all users, automation, and components in the Kubernetes cluster. The API server implements a RESTful API over HTTP, performs all API operations, and is responsible for storing API objects into a persistent storage backend. [3]

*Kubelet*: The kubelet is the primary “node agent” that runs on each node. It can register the node with the apiserver. [4]

## Kubernetes Concepts Series [1]

### Pods, Labels&Selectors, Replicas [1a]

Resource [1a] is the first in a series that explains Kubernetes deployments, services, secrets, config maps, daemon sets, and jobs.

The need for understanding the concepts: I like the second paragraph, that states what I think, too: that vanilla Docker is totally different from Kubernetes, and that in order to become efficient at it, you need to understand the concepts, not just the commands. This promises to be a good series!

*Pod*: A single deployable unit that is a collection of (Docker) containers that run in a single space, sharing the same virtual host. All containers in a pod are deployed on a single node and share the network and data volumes.

*Pod Bundles One App*: The intended use case of a pod is bundle together a container that does a main task with other helper containers, needed either to assist the main container (e.g. a proxy or an initializer), either to assist the maintainer (e.g. logging, data change watchers, etc.).

*Pod is Ephemeral*: A pod will not be rescheduled to a new node once it dies. If we want to keep a pod alive, we need _replica sets_.

*Label*: A key-value piece of information that has a user-defined meaning and can be attached to most of Kubernetes objects, including pods and nodes. Labels can be used within _selectors_ to select objects that share a common meaning. Examples of meanings: dev/production, ownership, release.

*Selector*: A mechanism for selecting objects based on labels. A key and one or more values of labels can be specified.

*Selector is the Core Grouping Primitive*: Using label selectors a client or user can identify and subsequently manage a group of objects. This is the core grouping primitive of Kubernetes and used in many places. One example of its use is working with _replica sets_.

*Replica Set*: A selection of pods, each of which being guaranteed the given number of replicas. The selection is made using _selectors_. If not using a replica set, a pod is not rescheduled if its node dies.

### Deployments [1b]

*Imperative vs Declarative Deployment*: The _imperative_ way is to deploy things manually, then adding other configurations (such as replica controllers) on top of them. The _declarative_ way is to create a declarative definition of how things should be deployed and configured, then execute that definition in order to deploy and manage the things. The _imperative_ way is good for learning and trying things out. The _declarative_ way is the way to go once things have settled.

*Deployment*: A declarative object used to automate deploying and rolling updates of applications. It allows easy scaling horizontally, easy progressive updates without service outage (by adding more replicas temporarily), rolling back updates in case of error. [5]

### Services [1c]

*Service*: An abstraction which defines a logical set of Pods and a policy by which to access them. The set of Pods targeted by a Service is usually determined by a selector. A _service_ is a REST object that can be deployed and managed.

*Service without Selector*: A _service_ used to abstract backends that are not part of Kubernetes.

*Service Discovery*: Query the _API server_, use a _service proxy_, _ExternalName_.

*Service Proxy*: Virtual IP for services, implemented by `kube-proxy`.

*Service ExternalName*: Type of service that maps to a DNS name rather than Pods.

### ConfigMaps and Secrets [1d]

*ConfigMap*: Configuration data, that stores key-value pairs. A value can be a string, a config file, or a blob. Used to decouple configuration from containers.

*Secret*: Configuration data similar to _ConfigMap_ but designed for small amounts of secret data, such as passwords, keys, etc. Kubernetes treats secrets different from _ConfigMap_'s: they are stored in a tmpfs and only on nodes that have pods that use them. However, they are passed to and from _API server_ in plain text, so a TLS connection is needed between _API server_ and _kubelet_ / user.

## Configure Liveness, Readiness and Startup Probes [6]

*Probe*: A mechanism by which the _kubelet_ determines certain states of a Container.

*Liveness Probe*:  _Probe_ used to determine when to restart a Container. Useful to catch a deadlock or a dead thread, where an application is running, but unable to make progress.

*Readiness Probe*: _Probe_ used to determine when a Container is ready to start accepting traffic. A Pod is considered ready when all of its Containers are ready. When a Pod is not ready, it is removed from _Service_ load balancers.

*Startup Probe*: _Probe_ used to determine when a Container application has started. It disables _liveness_ and _readiness_ probes until it succeeds, giving a chance to slow starting containers to be up and running when the other probes start.



# Technical Info

## How to Configure Secrets for Pull from Private Repository [7, 8]

The error `Error: ErrImagePull` from `kubectl describe pod`  may mean that you're trying to pull from a private repository.

In this case you need to create the secret, assuming that you've already logged in locally with `docker login`:

	kubectl create secret generic dockerhubcred --from-file=.dockerconfigjson=/Users/sorel/.docker/config.json --type=kubernetes.io/dockerconfigjson

This may not work if the local docker uses a credential manager from the OS.
In this case, create it with explicit credentials:

	kubectl create secret docker-registry dockerhubcred --docker-server=<your-registry-server> --docker-username=<your-name> --docker-password=<your-pword> --docker-email=<your-email>

(<your-registry-server> would be https://index.docker.io/v1/ for DockerHub.)

While doing this, make sure nobody's watching over your shoulder and once finished remove the command from your shell history.

Then refer it in your pod:

	apiVersion: v1
	kind: Pod
	metadata:
	name: microservice
	spec:
	containers:
	- name: microservice
		image: sorelmitra/microservice:latest
	imagePullSecrets:
	- name: dockerhubcred

## How to Attach to a Running Container

Assuming you have bash:

	kubectl exec -it POD-NAME -c CONTAINER-NAME bash

It's similar to `docker exec -it CONTAINER-NAME WHAT_EVER_LOCAL_COMMAND`.

## Considerations for Kubernetes Probes

### Liveness Probes

The purpose of a liveness probe is to tell Kube that the container is alive. How to determine if a container is alive from inside it depends a lot on what that container is doing and how. Below are a few items to consider when designing liveness probes in your container:

- *Healthy threads*: This means:
	- *All threads are alive*: If a thread crashes in Java, it does not necessarily crash the entire program. Your container is probably not alive, or at least not sane if one of its thread crashed.
	- *No deadlocks in threads*: If some threads are deadlocked, the container is not alive.

- *All external dependencies are alive*: This includes any dependency outside your program, such as a message queue, a store of any kind (i.e. DB, data grid). If such a dependency is not alive, the program will most certainly not function properly. It will be a design decision whether to report this as the program not being alive: for example we might want to restart a pod after it has tried to access its dead external dependency for N times, in the hope that this might mitigate a bug in that area of the code.

