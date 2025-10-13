# Object Oriented Programming

Design patterns are divided into three fundamental groups.

## Behavioral Patterns

Behavioral patterns are concerned with algorithms and the assignment of responsibilities and communication between objects.

- *Chain of responsibility*: Command objects are handled or passed on to other objects by logic-containing processing objects
- *Command*: Command objects encapsulate an action and its parameters
- *Externalize the Stack*: Turn a recursive function into an iterative one that uses a stack.
- *Hierarchical visitor*: Provide a way to visit every node in a hierarchical data structure such as a tree.
- *Interpreter*: Implement a specialized computer language to rapidly solve a specific set of problems
- *Iterator*: Iterators are used to access the elements of an aggregate object sequentially without exposing its underlying representation
- *Mediator*: Provides a unified interface to a set of interfaces in a subsystem
- *Memento*: Provides the ability to restore an object to its previous state (rollback)
- *Null Object*: designed to act as a default value of an object
- *Observer*: aka Publish/Subscribe or Event Listener. Objects register to observe an event that may be raised by another object
- *Weak reference*: De-couple an observer from an observable.
- *Protocol stack*: Communications are handled by multiple layers, which form an encapsulation hierarchy.
- *Scheduled-task*: A task is scheduled to be performed at a particular interval or clock time (used in real-time computing)
- *Single-serving visitor*: Optimize the implementation of a visitor that is allocated, used only once, and then deleted
- *Specification*: Recombine Business logic in a boolean fashion
- *State*: A clean way for an object to partially change its type at runtime
- *Strategy*: Algorithms can be selected on the fly
- *Template method*: Describes the program skeleton of a program
- *Visitor*: A way to separate an algorithm from an object

## Creational Patterns

Creational patterns are used to create objects for a suitable class that serves as a solution for a problem.

- *Factory Method*: When a client object does not know which class to instantiate, it can make use of the factory method to create an instance of an appropriate class from a class hierarchy or a family of related classes. The factory method may be designed as part of the client itself or in a separate class. The class that contains the factory method or any of its subclasses decides on which class to select and how to instantiate it.
- *Singleton*: Provides a controlled object creation mechanism to ensure that only one instance of a given class exists.
- *Abstract Factory*: Allows the creation of an instance of a class from a suite of related classes without having a client object to specify the actual concrete class to be instantiated.
- *Prototype*: Provides a simpler way of creating an object by cloning it from an existing (prototype) object.
- *Builder*: Allows the creation of a complex object by providing the information on only its type and content, keeping the details of the object creation transparent to the client. This allows the same construction process to produce different representations of the object.

## Structural Patterns

Structural patterns form larger structures from individual parts, generally of different classes.

- *Decorator*: Extends the functionality of an object in a manner that is transparent to its clients without using inheritance.
- *Adapter*: Allows the conversion of the interface of a class to another interface that clients expect. This allows classes with incompatible interfaces to work together.
- *Chain of Responsibility*: Avoids coupling a (request) sender object to a receiver object. Allows a sender object to pass its request along a chain of objects without knowing which object will actually handle the request.
- *Facade*: Provides a higher-level interface to a subsystem of classes, making the subsystem easier to use.
- *Proxy*: Allows a separate object to be used as a substitute to provide controlled access to an object that is not accessible by normal means.
- *Bridge*: Allows the separation of an abstract interface from its implementation. This eliminates the dependency between the two, allowing them to be modified independently.
- *Virtual Proxy Facilitates*: The mechanism for delaying the creation of an object until it is actually needed in a manner that is transparent to its client objects.
- *Counting Proxy*: When there is a need to perform supplemental operations such as logging and counting before or after a method call on an object, recommends encapsulating the supplemental functionality into a separate object.
- *Aggregate Enforcer*: Recommends that when an aggregate object is instantiated, all of its member variables representing the set of constituting objects must also be initialized. In other words, whenever an aggregate object is instantiated it must be constructed in full.
- *Explicit Object Release*: Recommends that when an object goes out of scope, all of the system resources tied up with that object must be released in a timely manner.
- *Object*: Cache Stores the results of a method call on an object in a repository. When client objects invoke the same method, instead of accessing the actual object, results are returned to the client object from the repository. This is done mainly to achieve a faster response time.

- https://www.gofpatterns.com/design-patterns/module1/intro-design-patterns.php

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

# Functional Programming

## Core Concepts

- **Purity**.  No side effects.  A function always returns the same output when called with the same argument values.
- **Immutability**.  A function has a disciplined state and does not change other variables outside of the function.  So you don't need to look at the code outside the function definition.
- **Disciplined state**.  No shared mutable state.  Almost stateless: you do have state, but only inside a function.
- **Higher-order functions**.  Functions are first-class citizens.  They can be passed as arguments, returned as values, stored in data structures, assigned to variables.
- **Referential transparency**.  A piece of code is has referential transparency if we can safely replace it with the value it computes and vice-versa.  This helps with refactoring and freeing mental space as we try to understand what the function does.
- **Currying**.  One parameter per function. If you need more than one argument, your function will return a new function that requires the next argument. You keep doing this until you no longer need any arguments.

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

