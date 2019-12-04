# Overview

This project aims to put together a schema that would simplify setting up and using automatic system testing frameworks for several types of projects.

I've named it "verifit" as a contraction of "Verify It (the system)!".

# Introduction

System testing (or application-level testing) is a must for every project. In a big company this is usually done by a dedicated QA team. But what if you're an individual working on a project or, if for any reason, there's no QA in your project?

Automatic system testing comes to rescue!

Usually you can't test a system 100% automatically. But, if you manage to cover the functionality (and perhaps some of the non-functional requirements, such as performance and high-availability - depending on the nature of the project), this will be a big help as the project builds up and you have more and more features to test as you make progress developing it.

Also, on small projects or where you're time constrained, having an automatic system testing framework that's easy to put to use on a daily basis is of great help.

# Goals

## Types of Projects

I need to set up a frame that would help me with automatic system testing of the following project types:

1. Web
	a. UI
	b. REST API
	c. WebSockets API
2. Mobile
	a. iOS
	b. Android
	(Ideally, cross-platform.)

## Setting Up the Framework(s)

The framework(s) I find need to be easily set-up in a new, time-constrained project.

Setting up the framework for that particular project needs to be somewhere along the lines of:

- Adding contents from an existing package.json to the project's package.json and running `npm install`
- Adding to a python's requirements.txt from an existing requirements.txt and running `pip install`

## Framework Language

Ideally I want all my testing for the required project types to be in a single language, preferably Python.
