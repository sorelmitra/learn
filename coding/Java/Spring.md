
Update: 2021-03-13
Most recent version used: https://spring.io/blog/2019/10/16/spring-boot-2-2-0
Includes Spring Framework 5.2.

# Overview

https://docs.spring.io/spring-framework/docs/5.0.0.RC2/spring-framework-reference/overview.html

## Definition

Spring is a Framework for Java that offers infrastructure that simplifies creating Java programs using POJOs.

## Getting Started

Manually: https://start.spring.io
IDEs such as IDEA and Eclipse use it in the background.

## Dependency Injection

Although Java provides a vast array of building blocks for an application, it lacks features to organize the code efficiently.  There are two ways to address this: implement yourself various design pattern, and resort to Spring's Inversion of Control.

Spring's Dependency Injection is a form of Inversion of Control which means that rather than having each object creating the objects it depends on, it injects those objects into it.

It works via the `Autowired` annotation and supports two forms: per variable or per constructor.

## Spring Framework Modules

+ __Core Container__

	- **Core** & **Beans**: `spring-core`, `spring-beans`: Fundamental parts of the framework, including Dependency Injection and `BeanFactory`, a sophisticated  factory pattern.
	
	- **Context**: `spring-context`: Offers Context, a directory-like means of accessing objects.  `spring-context-support` helps integrating third party libraries like EhCache or Quartz.
	
	- **SpEL**: `spring-expression`: Manipulating the object graph at runtime based on a language that supports most common operations on objects.

+ __AOP, Instrumentation & Messaging__

	- **AOP**: `spring-aop` offers aspect-oriented programming.  `spring-aspects` provides integration with AspectJ.
	
	- **Instrumentation**: `spring-instrument` offers class instrumentation support.
	
	- **Messaging**: Various messaging systems can be integrated into Spring.

+ __Data Access / Integration__
	- **JDBC**: `spring-jdbc` offers JDBC abstraction layer.
	- **Transactions**: `spring-tx` module provides a consistent programming model over different transaction APIs.  Supports declarative transactions.
	- **ORM**: `spring-orm` offers support for object-relational mapping such as `JPA` and `Hibernate`.
	- **OXM**: `spring-oxm` has object/XML mapping, e.g. JAXB.
	- **JMS**: `spring-jms` offers features for producing and consuming messages.

+ __Web__
	- **Web**: `spring-web` provides basic Web features for Servlets and an HTTP client.
	- **Servlet / MVC**: `spring-webmvc` has an MVC and REST implementation.
	- **WebSocket**: `spring-websocket` offers WebSockets support.

- **Test**: Module `spring-test`. Offers unit and integration testing of Spring components with JUnit or TestNG.  Loads Spring ApplicationContexts.  Has mock objects.

## Spring Security

Spring Security is a powerful and customizable authentication and authorization framework. It is the de-facto standard for securing Spring-based applications.

Key Features:

- Comprehensive and extensible support for both Authentication and Authorization
- Protection against attacks like session fixation, clickjacking, cross site request forgery
- Servlet API integration
- Integration with Spring Web MVC

## Spring Data

Spring Data provides a Spring-based programming model for data access while still retaining the special traits of the underlying data store.

It makes it easy to use data access technologies, relational and non-relational databases, map-reduce frameworks, and cloud-based data services. This is an umbrella project which contains many sub-projects that are specific to a given data system.

Features:

- Powerful repository and custom object-mapping abstractions
- Dynamic query derivation from repository method names
- Implementation domain base classes providing basic properties
- Support for transparent auditing (created, last changed)
- Possibility to integrate custom repository code
- Easy Spring integration via JavaConfig and custom XML namespaces
- Advanced integration with Spring MVC controllers
- Experimental support for cross-store persistence

Some modules:

- Spring Data Commons - Core Spring concepts underpinning every Spring Data module.
- Spring Data JDBC - Spring Data repository support for JDBC.
- Spring Data JPA - Spring Data repository support for JPA.
- Spring Data Redis - Easy configuration and access to Redis from Spring applications.
- Spring Data REST - Exports Spring Data repositories as hypermedia-driven RESTful resources.



# Spring Boot

https://spring.io/projects/spring-boot#overview

## Overview

Spring Boot provides an opinionated framework that makes it easy to create stand-alone, production-grade Spring based Applications that you can "just run".

It relies on conventions and provides ready-made structure and configuration that simplify building Java applications from scratch.  To use it, you have to align with its structure.

It includes various features of the Spring ecosystem, such as Framework and Security, plus integrations with 3rd party libraries, such as JUnit, ElasticSearch and Jackson.

There's a "starter" for typical cases of applications, such as:

- `spring-boot-starter-web` for Web apps, including RESTful apps.
- `spring-boot-starter-data-rest` for exposing Spring Data repos over REST.  Not to be confused with the Web starter that allows RESTful apps.
- `spring-boot-starter-security` for Spring Security.

Complete list on https://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#using-boot-starter

Typical list of things included in a Spring Boot release, below for 2.2.0:

- Spring AMQP 2.2
- Spring Batch 4.2
- Spring Data Moore
- Spring Framework 5.2
- Spring HATEOAS 1.0
- Spring Integration 5.2
- Spring Kafka 2.3
- Spring Security 5.2
- Spring Session Corn

