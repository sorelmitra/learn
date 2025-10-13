# Monolith

https://incident.io/blog/monolith
https://robertorodes.com/the-blog/the-majestic-monolith-demystified/
https://www.monolithic.dev
https://blog.koehntopp.info/2023/05/10/its-a-modulith.html
How to recover from micro-services: https://world.hey.com/dhh/how-to-recover-from-microservices-ce3803cc

In a monolithic architecture, the entire application is bundled into a single package that includes the binaries, configurations, and all other dependencies required for it to run.

## Advantages

- Everything is in one place, no need for managing multiple, different services, all with their own sets of databases, tooling, configurations, deployments, and so on.


## Disadvantages

- Hard to design in a modular way, the temptation is to stick everything together.

## Principles

1. Never mix workloads

A 'workload' is a single unit of work.  Example of a breakdown of the units of work for an imaginary web app:

- Web: Handles incoming requests and places them in a queue.
- Worker: Processes queued requests.
- Cron: Does scheduled maintenance.

(You might be tempted to separate the above into micro-services, but the advantage of keeping the monolith is its simplicity, because it's not distributed.)

You can now start your monolith in two ways:

As separate workloads in your production and test environments:

- Web: `myapp --web`.
- Worker: `myapp --worker`.
- Cron: `myapp --cron`.

Or as a single bundle, on your local machine (great developer experience):

- Bundle: `myapp --web --worker --cron`.

2. Communicate externally between workloads

Workloads don't coordinate internally in the same process, but by using external means, such as a Database or Message Queues.  This makes the system modular, resilient and easy to debug.

3. Use guardrails on limited resources

Because all your code is accessing the same limited resource, say, the database, apply sensible rules to prevent any of your workloads from stealing too much capacity of the limited resource.  Examples for a database:

- Maximum open and idle connections.
- Connection idle and active timeouts.
- Statement timeout.

## Distributed Monolith Anti-pattern

This problem relies in the fact that you try to distribute your app like a collection of micro-services or serverless functions but rather build tightly coupled and/or huge services that all access the same database, talk too much with each other or scale up poorly.

More explanation of what usually happens in the real world when doing serverless in an [AWS blog](https://aws.amazon.com/blogs/compute/best-practices-for-organizing-larger-serverless-applications/).  This blog is of course focused on AWS solutions, but then when you go to serverless you agree to be Service Provider-bound...
So with serverless you often end up with huge lambdas that make use of frameworks for which the Provider has better alternatives.

I saw once such an example of a huge lambda that was doing everything related to the products that were part of that company's offer.  That lambda was handling the typical CRUD operations, filtering, attachments, and all things product-related.  So now the real question arises, how do you do such a thing with lambdas?  Some points of discussion:

- With micro-services, accessing another service's DB is a bad pattern.  Does this apply to lambdas as well?
- Given that lambdas are really functions part of your code/app, it would make sense that they access the same DB - there's no clear boundary between different functions of a serverless app.
- Are serverless apps truly monoliths when it comes to data access and functions cooperation but "nano-services" when it comes to deployment, scalability and resiliency?
- This [article](https://blog.bitsrc.io/building-a-serverless-webapp-why-you-should-consider-the-monolith-4f0105935589) suggests using a real monolith lambda because "implementing each operation in a separate Lambda function [...] is an overkill in terms of separation for most of the practical use cases with Web Apps".
- In the end, as the original article on serverless from martinfowler.com suggests, I guess the answer is serverless is not for every type of a project.
- So now the hard question: how do you find out if serverless is for your project or not?

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

# Micro-Services

Sources:
https://micro-services.io/patterns/micro-services.html
https://nordicapis.com/the-business-caveats-of-micro-services/

## Definition

The word “micro-service” refers to the individual services in a micro-service architecture. A micro-service architecture is an architectural style for modern web apps where the functionality is broken up into smaller fragments.

The micro-services architecture tackles complexity by using the ancient technique of modularization.  Each micro-services corresponds to a business capability.  Each service has its own DB which ensures decoupling.  A micro-service has well defined boundaries which enforce modularity.

## Overview

Suppose we have the following constraints:

- There is a team of developers working on the application
- New team members must quickly become productive
- The application must be easy to understand and modify
- You want to practice continuous deployment of the application
- You must run multiple instances of the application on multiple machines in order to satisfy scalability and availability requirements
- You want to take advantage of emerging technologies (frameworks, programming languages, etc)

Solution: Define an architecture that structures the application as a set of loosely coupled, collaborating services. This approach corresponds to the Y-axis of the Scale Cube.

## Advantages

**Each** service is:

- Highly maintainable and testable - enables rapid and frequent development and deployment
- Loosely coupled with other services - enables a team to work independently the majority of time on their service(s) without being impacted by changes to other services and without affecting other services
- Independently deploy-able - enables a team to deploy their service without having to coordinate with other teams
- Capable of being developed by a small team - essential for high productivity by avoiding the high communication head of large teams

Note the emphasis on 'each': these advantages apply to a particular service.  But as your system relies of the interoperability of these services, problems start to arise.

## Disadvantages

Distributed Systems are Hard: They come at a price, and the price is high.

- The various pieces that compose them interact in a complex way that's hard to manage.
- We need to make sure those pieces cooperate correctly.
- We need a lot more support to run them correctly, and a lot more tooling.
- We haven't solved any business problem yet — we need all that just to make it work somewhere.

Pick Your Distributed Poison
One of the hardest things for people to understand with distributed systems is that eventual consistency is the same thing as eventual inconsistency.
https://hazelweakly.me/blog/pick-your-distributed-poison/


## Distributed Data - Domain Driven Design

The problem with domain model and micro-services:

- The domain model is usually a tangled web of classes.  The micro-services boundaries make it difficult to reference objects from other micro-services.
- Reliance on ACID (atomicity, consistency, isolation, durability) transactions to enforce invariant and business rules.  But again this wants to create a transaction that in a micro-services architecture would need to span multiple micro-services, each with its own database.  This firstly violates encapsulation and secondly requires a distributed atomic transactions or two-phase commit protocol (2PC).  2PC transactions add complexity and although it guarantees consistency, there are issues you need to handle and it's not supported by NoSQL and other technologies. 
- CAP Theorem: It's impossible for a distributed data store to simultaneously provide more than two out of the following three guarantees:
	- Consistency: Every read receives the most recent write or an error
	- Availability: Every request receives a (non-error) response, without the guarantee that it contains the most recent write
	- Partition tolerance: The system continues to operate despite an arbitrary number of messages being dropped (or delayed) by the network between nodes

The solution? [Domain Driven Design by Eric Evans](https://learning.oreilly.com/library/view/domain-driven-design-tackling/0321125215/)!  In a nutshell: The building blocks of domain-driven design are:

- *Entity*: An object defined primarily by identity.  E.g., a kid that draws.
- *Value Object*: An object that represents a descriptive aspect of the domain with no conceptual identity.  E.g. a pencil of a particular color used by the kid that draws.
- *Service*: An operation offered as an interface that stands alone in the model, without encapsulating state.  E.g. the act of drawing by the kid.
- *Repository*: An abstraction of common operations on persisted entities and value objects.
- *Aggregate*: A graph of related entities and value objects where:
	- An entity sits at the root
	- Each entity references other entities by is primary key
	- Its granularity is small enough to favor user experience and availability
	- A transaction operates on a single aggregate
	- Consistency is ensured by using different techniques based on events, one of which is Event Sourcing

The _aggregate_ is the key to the above problems as it can be stored separately inside a micro-service, it can be updated in a single separated transaction, and references to other entities are done by primary key, thus loosely coupling it with referenced objects.  When a service updates or fails to update its aggregate it announces that via events.

*Event Sourcing* means that instead of storing actual entities in a database, you just store the events.  Then loading all events of a particular type reconstructs a particular object.

- https://youtu.be/7kX3fs0pWwc

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

# Self-Contained Systems

Sources:
https://scs-architecture.org

## Definition

The Self-contained System (SCS) approach is an architecture that focuses on a separation of the functionality into many independent systems, making the complete logical system a collaboration of many smaller software systems. This avoids the problem of large monoliths that grow constantly and eventually become unmaintainable.

## Common Things with micro-services


The SCS approach shares a lot of concepts with micro-services, including:

- Enforcing isolation via independently deployable units.
- Alignment of organizational and architectural boundaries
- Support for diversity in terms of technology choices.
- Lack of centralized infrastructure.

If we accept this as the core of micro-services, we could view SCSs as a specialization.  But compared to some other aspects seen by many people as key attributes of micro-services, there are some important differences, outlined below.

## Differences from micro-services

- A micro-service is probably smaller than an SCS.  An SCS might be large enough to keep a team busy and provide more meaningful business value.
- There are usually fewer SCSs than micro-services.  A logical system such as an e-commerce shop might have 5 to 25 SCSs i.e. for billing, order processing etc.  An e-commerce shop might have 100s of micro-services.
- SCSs should ideally not communicate with each other while this is fine for micro-services.
- SCSs have a UI, while micro-services might separate the UI from the logic in its own service.  (However, some definitions of micro-services include the UI in the micro-service, too.)
- SCSs should favor integration at the UI layer.  micro-services typically focus on integration at the logic layer.  (Again, some definitions of micro-services also allow for integration at the UI layer.)
- Of course it is possible to split an SCS even further so it consists of micro-services — in particular for the business logic. In this case, this can be seen as a particular micro-architecture approach.
- SCS clearly focus on larger projects and a split into multiple teams. micro-services can be used for other purposes: Often small teams or even single developers use them e.g. to use Continuous Delivery more easy, to build more robust systems or to scale each micro-service independently. So micro-services are more versatile while SCS solve problems specifically with the architecture and organisation of large projects.

## Characteristics

An SCS:

- Is an **autonomous web application** for a particular domain.  No need for other systems to be available for it to function.
- Includes both **data** and **business logic**.  To really implement any meaningful features both are needed.
- Has a business logic that **only implements that SCS' domain**, and is not shared with other SCSs.  Common libraries are fine.
- Should have **separate database**, with **redundant copies** of data owned by other SCSs if need be.  It **may share database** though, for costs or other reasons.
- Exposes its own **web UI**, that is built according to [ROCA Principles](https://roca-style.org).  No shared UI with other SCSs, although **links**, **redirection**, and **inline content** are fine.  The UI works even if the links are not functional.
- May have an **optional service API**, e.g. for mobile clients or for other SCSs.
- Communicates with other systems **asynchronously**, via **RESTful** APIs or **lightweight messaging**.  This decouples the systems, reduces the effects of failure, and thus supports autonomy.
- Is **owned by one team**.  The owning team has the final say on what goes into the code base, while others are welcome to contribute.
- Has its own **technical stack**: programming language, tools, frameworks, infrastructure.  Different SCSs can use different stacks.

## Advantages

- Improves **resiliency** because the SCSs are loosely coupled.
- Supports **scaling** of individual SCSs.
- **Migration** from complex architectures to SCSs can happen **incrementally** by extracting one SCS at a time.

## Disadvantages

- Defining autonomous SCSs can be a challenge when API calls to other services want to be minimized.

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

# Serverless Computing

Sources:
https://martinfowler.com/articles/serverless.html
https://www.twilio.com/docs/glossary/what-is-serverless-architecture

References:
https://blog.symphonia.io/posts/2017-06-22_defining-serverless-part-1
https://samnewman.io/books/building_micro-services/
https://gojko.net/2017/10/05/serverless-design-gotocph.html

## Definition

Serverless applications are ones that are implemented using serverless services. A serverless service is one that entirely, or very nearly entirely, exhibits five common traits:

- Requires no management of Server hosts or Server processes (explained below)
- Self auto-scales and auto-provisions, based on load
- Offers costs based on precise usage
- Has performance capabilities defined in terms other than host size / count
- Has implicit High Availability

Serverless can mean one of two (overlapping) things:

1. BaaS ([Mobile] Backend as a Service), in which rich client apps (e.g. mobile, single page web) rely on ready-available cloud backend services (e.g. DB: Parse, Firebase; Authentication: Auth0, AWS Cognito).  The main difference from PaaS is that you basically don't write any server-side code.  It's frequently used along with FaaS.
2. FaaS (Functions as a Service), where you do write server-side code but with a different architecture than both BaaS and PaaS: it's run in stateless compute containers that are event-triggered, ephemeral (may only last for one invocation), and fully managed by a third party and can be scaled automatically as function call frequency increases or decreases.  You typically pay per usage in this case.

In a typical client-server app, all flow, control, and security is managed by the central server application. In the serverless version there is no central arbiter of these concerns. Instead we see a preference for choreography over orchestration, with each component playing a more architecturally aware role — an idea also common in a micro-services approach.

## Serverless vs PaaS

PaaS, or Platform as a Service, products such as Heroku, Azure Web Apps and AWS Elastic Beanstalk offer many of the same benefits as serverless (sometimes called Function as a Service or FaaS). They do eliminate the need for management of server hardware and software. The primary difference is in the way you compose and deploy your application, and therefore the scalability of your application.

With PaaS, your application is deployed as a single unit and is developed in the traditional way using some kind of web framework like ASP.NET, Flask, Ruby on Rails, Java Servlets, etc. Scaling is only done at the entire application level. You can decide to run multiple instances of your application to handle additional load.

This supports both Monolithic and micro-services architecture.

Is serverless the same as PaaS? 

"If your PaaS can efficiently start instances in 20ms that run for half a second, then call it serverless." (Adrian Cockcroft.) In other words, most PaaS applications are not geared towards bringing entire applications up and down in response to an event, whereas FaaS platforms do exactly this.

## Advantages

- Systems built this way are often more flexible and amenable to change, both as a whole and through independent updates to components
- There is better division of concerns
- There are also some cost benefits

## Disadvantages

- It requires better distributed monitoring
- We rely more significantly on the security capabilities of the underlying platform
- More fundamentally, there are a greater number of moving pieces to get our heads around than there are with the monolithic application we had originally - Whether the benefits of flexibility and cost are worth the added complexity of multiple backend components is very context dependent
- If your traffic is uniform and would consistently make good utilization of a running server you may not see this cost benefit, and you may actually spend more by using FaaS - do your math before choosing
- Multi-tenancy problems - depending on vendor
- Vendor lock-in - developing and operating applications in a way that's agnostic of the actual cloud vendor being used is quite costly
- Security concerns: direct database access, security config per-function
- Repetition of logic across client platforms - each client platform needs to implement same logic
- No in-server state for serverless FaaS - external options such as Redis are still much slower than in-server storage
- Implementation drawbacks:
	- Configuration of functions (depending on vendor)
	- DoS attack your self (limits on # of concurrent functions running)
	- Execution duration
	- System & load testing - need the same cloud for that
	- Deployment, packaging, and versioning
	- Monitoring and observability
	- Deferring of operations (dev-ops) until they hit you hard

## FaaS

### Definition

Fundamentally, FaaS is about running backend code without managing your own server systems or your own long-lived server applications.

### Development

1. FaaS support coding using common programming languages. Typically such a function can also execute another process that is bundled with its deployment artifact, so you can actually use any language that can compile down to a Unix process.
2. Deployment is very different from traditional systems since we have no server applications to run ourselves. In a FaaS environment we upload the code for our function to the FaaS provider, and the provider does everything else necessary for provisioning resources, instantiating VMs, managing processes, etc.

### Architectural Concerns

1. Triggers. Functions in FaaS are typically triggered by event types defined by the FaaS provider. Most providers also allow functions to be triggered as a response to inbound HTTP requests, typically by way of using an API gateway.

2. Scaling. Horizontal scaling is completely automatic, elastic, and managed by the provider. The vendor handles all underlying resource provisioning and allocation — no cluster or VM management is required by the user at all.

3. Stateless? Local storage is not usable. Persistent storage is available externally to the function itself, which might incur delays.

4. Limited execution duration. Code architecture must respond to that.

5. Startup latency. A function may be invoked with a "cold start" - creating all the infrastructure in order to run it, or "warm start" - reusing the environment from a previous invocation. Cold starts take time and this must be taken into account when designing the system.

6. API gateway. This is an HTTP server where routes and endpoints are defined in configuration, and each route is associated with a resource to handle that route. In a serverless architecture such handlers are often FaaS functions. Typically the API gateway will allow mapping from HTTP request parameters to a more concise input for the FaaS function, or will allow the entire HTTP request to be passed through, typically as a JSON object. The FaaS function will execute its logic and return a result to the API gateway, which in turn will transform this result into an HTTP response that it passes back to the original caller. API gateways may also perform authentication, input validation, response code mapping, and more.

## Tech Stack Examples

See `learn/javascript/serverless-ts/Readme.md` / Architecture.

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

