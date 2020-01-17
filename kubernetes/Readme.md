# Introduction

Trying to learn about Kubernetes.

A Kube playground: https://labs.play-with-k8s.com/

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

