# Containers

Virtualization has some advantages over traditional hardware systems:

- Gain better performance and efficiency from resources in the existing computing components, using CPU virtualization.
- Boost virtual machine (VM) security. Since VMs are logically separated from each other, a malware attack or other software glitch on one VM won't affect other VMs.
- Save money on hardware. Virtualization software involve less cost, and also require lesser hardware to run than physical machines.
- Gain peace of mind. VMs provide better reliability in terms of disaster recovery as well as better backup and retrieval capabilities.

- https://www.manageengine.com/network-monitoring/what-is-virtualization.html

But there are some challenges with virtualization approaches, namely:

- Environment consistency – Deployment of applications/packages to VMs
- OS dependency – Deployed apps are able to run on compatible OS only
- Isolation level – Unable to provide instant Sandbox above OS level
- Compute consumption granularity – Unable to deploy multiple replicated apps and load balance on application layer only within the single machine (not OS layer)
- Patching of images in production-grade environments – blue-green and canary deployments are not flexible on a cluster level and are hard to manage across multiple regions

Containerization has become a major trend in software development as an alternative or companion to virtualization. It involves encapsulating or packaging up software code and all its dependencies so that it can run uniformly and consistently on any infrastructure.  

Benefits

- Reduced cost of infrastructure operations – There are usually many containers running on a single VM
- Solution scalability on the micro-service/function level – No need to scale instances/VMs
- Better security – Full application isolation makes it possible to set each application's major process in separate containers
- Instant replication of micro-services via replicas and deployment sets
- Flexible routing between services that are natively supported by containerization platforms
- Deploy anywhere – Including hybrid environments
- Full portability between clouds and on-premises locations
- OS independent – They don't need an OS to run; only the container engine is deployed on a host OS
- Fast deployment with hydration of new containers and termination of old containers with the same environments
- Lightweight – Without an OS, containers are lightweight and less demanding on server resource usage than images
- Faster “ready to compute” – Containers are ready to start and stop within seconds in comparison to VMs.

- https://www.capgemini.com/2019/11/the-top-benefits-of-containerization/

---
---
---
---
---
---
---
---
---
---
---
---

# Cloud

The term Cloud refers to a Network or Internet. In other words, we can say that Cloud is something, which is present at remote location. Cloud can provide services over public and private networks, i.e., WAN, LAN or VPN.

Cloud Computing refers to manipulating, configuring, and accessing the hardware and software resources remotely. It offers online data storage, infrastructure, and applications.

Access types:

- Public Cloud: allows systems and services to be easily accessible to the general public. Public cloud may be less secure because of its openness.
- Private Cloud: allows systems and services to be accessible within an organization. It is more secured because of its private nature.
- Community Cloud: allows systems and services to be accessible by a group of organizations.
- Hybrid Cloud: a mixture of public and private cloud, in which the critical activities are performed using private cloud while the non-critical activities are performed using public cloud.

Service models:

- Infrastructure-as-a-Service (IaaS) is the most basic level of service and provides access to fundamental resources such as physical machines, virtual machines, virtual storage, etc.

- Platform-as-a-Service (PaaS) provides the runtime environment for applications, development and deployment tools, etc.

- Software-as-a-Service (SaaS) allows to use software applications as a service to end-users.

- https://www.tutorialspoint.com/cloud_computing/cloud_computing_overview.htm

---
---
---
---
---
---
---
---
---
---
---
---

