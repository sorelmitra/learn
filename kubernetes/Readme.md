# Introduction

Trying to learn about Kubernetes.

# Resources

[1] A series of [1a-e], explaining Kubernetes Concepts

[1a] https://blog.giantswarm.io/understanding-basic-kubernetes-concepts-i-introduction-to-pods-labels-replicas/

[1b] https://blog.giantswarm.io/understanding-basic-kubernetes-concepts-using-deployments-manage-services-declaratively/

[1c] https://blog.giantswarm.io/basic-kubernetes-concepts-iii-services-give-abstraction/

[1d] https://blog.giantswarm.io/understanding-basic-kubernetes-concepts-iv-secrets-and-configmaps/

[1e] https://blog.giantswarm.io/understanding-basic-kubernetes-concepts-v-daemon-sets-and-jobs/

[2] http://kubernetesbyexample.com - a good companion to [1]

# Lecture Notes

## Kubernetes by Example [2]

This is oriented towards demonstrating Kubernetes concepts with examples. It also explains a bit those concepts, but it's most value comes from the examples. It is a good companion to [1].

## Kubernetes Concepts Series [1]

### Pods, Labels&Selectors, Replicas [1a]

Resource [1a] is the first in a series that explains Kubernetes deployments, services, secrets, config maps, daemon sets, and jobs.

The need for understanding the concepts: I like the second paragraph, that states what I think, too: that vanilla Docker is totally different from Kubernetes, and that in order to become efficient at it, you need to understand the concepts, not just the commands. This promises to be a good series!

*Pod*: A single deployable unit that is a collection of (Docker) containers that run in a single space, sharing the same virtual host. All containers in a pod are deployed on a single node and share the network and data volumes.

*Pod Bundles One App*: The intended use case of a pod is bundle together a container that does a main task with other helper containers, needed either to assist the main container (e.g. a proxy or an initializer), either to assist the maintainer (e.g. logging, data change watchers, etc.).

*Pod is Ephemeral*: A pod will not be rescheduled to a new node once it dies. If we want to keep a pod alive, we need _replica sets_.
