# Concurrency & Parallelism

## Concurrency

Concurrency means that an application is making progress on more than one task, and is doing this concurrently, i.e. the tasks compete with each other for CPU time.  To make progress on more than one task concurrently the CPU switches between the different tasks during execution:

	CPU      ===        ======         task 1
	             ======        =====   task 2

## Parallel Execution

Parallel execution is when a computer has more than one CPU or CPU core, and makes progress on more than one task at the same time:

	CPU 1      =========         task 1
	CPU 2      ===========       task 2

However, parallel execution is not parallelism.  It's just about a computer's ability of running things in parallel.

## Parallel Concurrent Execution

It is possible to have parallel concurrent execution, where threads are distributed among multiple CPUs. Thus, the threads executed on the same CPU are executed concurrently (task 1 & task 2), whereas threads executed on different CPUs are executed in parallel (task):

	CPU 1    ===        ======         task 1
	             ======        =====   task 2
	CPU 2      ====        =====       task 3
	                ======       ===   task 4

## Parallelism

The term parallelism means that an application splits its tasks up into smaller subtasks which can be processed in parallel, for instance on multiple CPUs at the exact same time. Thus, parallelism is about intentional design of an app to do processing in parallel.  To achieve true parallelism your application must have more than one thread running - and each thread must run on separate CPUs / CPU cores / graphics card GPU cores or similar.

Below a bigger task is being split up into 4 subtasks. These 4 subtasks are being executed by 4 different threads, which run on 2 different CPUs. This means, that parts of these subtasks are executed concurrently (those executed on the same CPU), and parts are executed in parallel (those executed on different CPUs).

	CPU 1    ===                              app
	          |
	          ->    ===        ======         subtask 1
	          ->        ======        =====   subtask 2
	CPU 2     ->      ====        =====       subtask 3
	          ->           ======       ===   subtask 4

If instead the 4 subtasks were executed by 4 threads running on each their own CPU (4 CPUs in total), then the task execution would have been fully parallel. However, it is not always easy to break a task into exactly as many subtasks as the number of CPUs available. Often, it is easier to break a task into a number of subtasks which fit naturally with the task at hand, and then let the thread scheduler take care of distributing the threads among the available CPUs.

- http://tutorials.jenkov.com/java-concurrency/concurrency-vs-parallelism.html

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

