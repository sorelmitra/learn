# Communication to the Front-End

The front-end (FE) will be calling to at least one backend in order to retrieve and update its data.  Some things to have in mind:

- The FE needs to always call the source of truth for each data, to avoid issues with eventual consistency in the case of multiple services and event-based architecture.  I.e., after the FE triggers an update to one service, it needs to re-fetch the relevant data from its sources of truth, rather than from some centralized place that might not be updated right after the update.

- Consider creating a unified backend, that will act as a single point of contact for the FE.  This has the advantage of simplifying the FE logic and development, and the disadvantage of maintaining another API.

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

# Communication Between Services

## APIs vs. WebSockets vs. WebHooks

https://blog.bitsrc.io/apis-vs-websockets-vs-webhooks-what-to-choose-5942b73aeb9b

## RESTful APIs

Best practices for REST API design
https://stackoverflow.blog/2020/03/02/best-practices-for-rest-api-design/

Why PATCH is Good for Your HTTP API
- Talks about the https://www.rfc-editor.org/rfc/rfc6902 version of PATCH, which is pretty complex and cumbersome to work with
https://www.mnot.net/blog/2012/09/05/patch
https://stackoverflow.com/a/21919386/6239668

Please. Don't Patch Like That.
- Another adept of RFC6902
https://williamdurand.fr/2014/02/14/please-dont-patch-like-that/

RESTful API Design — PUT vs PATCH
- Simple PATCH, in which you just send the resource to update
- My preferred way
https://medium.com/backticks-tildes/restful-api-design-put-vs-patch-4a061aa3ed0b

JSON Merge Patch
- The same simple PATCH, but formalized
https://datatracker.ietf.org/doc/html/draft-ietf-appsawg-json-merge-patch-02

## GraphQL
https://www.howtographql.com

# Web Services

A Web service is a software system designed to support interoperable machine-to-machine interaction over a network. It has an interface described in a machine-processable format (specifically WSDL). Other systems interact with the Web service in a manner prescribed by its description using SOAP-messages, typically conveyed using HTTP with an XML serialization in conjunction with other Web-related standards.

- https://nordicapis.com/what-is-the-difference-between-web-services-and-apis/

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

# Messaging & Event Streaming

## Core Terms

What is an Event?

- An event encapsulates a change in state (what has happened)
- It should be lightweight and carry only enough information required about the change in state
- An event is distributed to notify any interested parties
- Events are distributed through channels such as streaming and messaging

An example, when a consumer borrows a DVD, the DVD state changes from “for rent” to “rented”.

What is a Message?

- A message encapsulates the intention / action (what has to happen)
- It is not lightweight containing a payload of all data required for processing
- A message can be formatted and adhere to a contract to be suitable for the interested parties
- A message is distributed through channels such as messaging

An example, when a customer places a reservation on a DVD but requests it be collected from another location, the booking system receives a message to reserve the stock and trigger a transfer for the DVD to another location.

What is Event Sourcing?

- Event sourcing is a way to atomically update state of an object.
- The traditional way to persist an entity is to save its current state.
- Event sourcing is about a business object being persisted by storing a sequence of events, each event representing a state change.  Whenever an object's state changes, a new event is appended to the sequence of events. Since that is one operation it is inherently atomic.  A entity's current state is reconstructed by replaying its events.

What is a Stream?

- A stream consists of immutable data, only inserting new events, whereas existing events cannot be changed.
- Streams are persistent, durable and fault tolerant.

What is Streaming?

- Streaming of data is the constant flow of events where each event should contain enough information to reflect the change in state.
- It allows for the processing of data to occur in real-time (data in motion) and is different from the traditional approach for the processing of static data to occur (data at rest) at a later point in time, known as batch processing
- Streaming data is unbounded, meaning it has no real beginning and no real end
- Each event is processed as it occurs and is managed accordingly

An example of this can be found in the stock market.  When a stock price change, a new event is created containing the time and day, the stock identifier, and its new trade price which in this example is the change in state.  Given there are thousands of stocks, and thousands of trades happening every second, this results in a constant stream of data.

What is Publisher (pub)?

- An application that publishes a message to a topic
- It does not know the destination of the message

What is Subscriber (sub)?

- An application that registers itself to a topic to receive the messages
- It does not know the source of the message

What is a Queue?

- A Queue is a mechanism for system to system communication that is asynchronous in behaviour
- Queues store messages until they are processed and deleted.
- Each message is processed only once and for a single consumer.
- Queues can be used to decouple heavyweight processing, to control the flow of an influx or batch data, and to support erratic workloads.
- This is typically used in serverless and micro-services architectures.

What is a Topic?

- Is a channel that maintains a list of subscribers to relay messages to that are received from publishers

## Event Streaming

Event streaming is really powerful when you have events that you want to be able to process and perform analysis in real-time allowing your systems to immediately take action.

- Event streaming is used for real-time processing and analysis of changes in state in the data through events
- It can persist events supporting the rebuilding of the state through replaying events in the order they were received
- It allows multiple consumers to receive each event

## Messaging

Messaging is powerful when it comes to decoupling your systems and is a way to manage your system interactions and control the ingestion of your data.

### Pub-Sub (Publish-Subscribe)

- Pub-sub moves data from producers to consumers
- It allows multiple consumers to receive each message in a topic
- Publish-subscribe ensures that each consumer receives messages in a topic in the exact order in which they were received by the messaging system

### Queueing

- Message queueing ensures that for exactly one consumer each message is delivered and processed
- It does not ensure that messages are delivered or processed in order
- However, each message is removed from the queue once it has been delivered but it does requires consumer acknowledgement

- https://robertleggett.blog/2020/03/02/choosing-event-streaming-or-messaging-for-your-architecture/

## Making a Decision

Ask yourself questions like these:

- Do you understand your use case?
- Do you understand how this may evolve?
- What does the roadmap for your product look like?

This will be the driver for the technology choice that you should use.

If you...

- Require processing and analysis of your data in real-time
- Have a constant flow of data changing in state that you may want to persist, replay or use to recreate your state

Then event streaming is the path to explore.

However, if you...

- Need a decoupled, highly durable and highly available architecture
- Need asynchronous execution
- Don't require real-time processing of your data
- Are issuing commands

Then messaging is the path to explore.

Does that mean that you can't combine these approaches?

Not at all, however event streaming is a much more complex setup and therefore my recommendation is if your use case won't evolve and require real-time processing and analysis of your data, then the complexity that event streaming adds to your solution most likely is not worth it.

Apache Kafka is a bit of both.

## WebHooks

A web hook is a way of notifying an app when an event happened, by means of calling an HTTP endpoint.

Because the endpoint you are calling might be down or slow, in order to avoid messing with your main code path, you can actually emit events in a message queue, and leave web hook calling to a queue event processor.

[1] https://medium.com/swlh/how-to-build-a-webhook-delivery-system-34778f7dd81

[2] https://webhookrelay.com/blog/2018/07/13/how-to-create-webhook/

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

