# Less is More

Complexity is the enemy: https://grugbrain.dev
Computers have became frustrating: https://annasofia.xyz/2022/11/05/criticizing-computers.html
Building simple systems: https://cr.yp.to/bib/1995/wirth.pdf

Monolith is simpler than distributed systems: https://incident.io/blog/monolith
Death By A Thousand Micro-services: https://renegadeotter.com/2023/09/10/death-by-a-thousand-microservices.html

Why we're leaving the cloud... https://world.hey.com/dhh/why-we-re-leaving-the-cloud-654b47e0
... We have left the cloud https://world.hey.com/dhh/we-have-left-the-cloud-251760fb

Code the shortest path first: https://evanlh.com/posts/shortest_path_first_sdlc/

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

# Checklists

## Project Estimation & Tracking Checklist

- Start a couple of initial sprints
	- Do some stories
	- Estimate them (post-doing?) as best as you can
	- Store them as references
- Define stories for the envisioned work scope
	- Identify inter-story dependencies if any, and link them in your issue-tracking system
	- These are the DEV stories
	- If you have a PO or customer that's creating non-workable stories, you can create the DEV stories in the same epic and link them as "must be done before" the PO stories
- Estimate in story points
	- Use previous stories as a reference
	- The purpose of story points is to identify size
	- Story point numbers are like [Dog Sizes](https://medium.com/serious-scrum/what-is-the-easiest-way-to-explain-story-points-a8ef01c816fb)
	- Story point numbers do NOT translate into days or hours; rather they identify size by finding the closest number that matches, that's what Dog Sizes are about - like "what dog out of the 5 we have is the closest one in size to the new dog that arrived" - chances are the new dog's size will not match exactly any of the 5 reference ones, so you'll have to choose the closest one
	- To avoid thinking about time, it helps thinking about the tasks involved instead, comparing them to the tasks involved in reference stories
	- Keep it small and simple, use small numbers 1, 2, 3, 5, 8; anything bigger, it's either an epic or will be broken down
	- You really want to stick with the 2, 3, 5 story sizes most of the time
	- Estimate quickly (you have reference stories!), without giving a specific time commitment (this is what agility is about!), embrace the uncertainty that comes with estimation (Fibonacci numbers help here), and accurate enough to plan sprints ahead
	- That's all that story points really are about
- Estimate for an entire big release needed?
	- Estimate all backlog in story points
	- Organize them in future sprints
	- Define a velocity based on the first two sprints
	- Use estimated velocity to count # of sprints
	- Add some buffer sprints
	- Give back total number of sprints
	- Don't forget QA sprints and non-dev work such as security testing, licensing and others
- QA work
	- Create a complete QA backlog with stories & other QA tasks
	- Each QA story corresponds to a DEV story, link the two in your issue-tracking system
	- If the DEV story is closed and you want to include it in a release:
		- If the corresponding QA story is closed, then you're good to go
		- Otherwise decide if you'll leave it out of the release or include it as an alpha or early-preview
	- Do QA estimation for each story
	- Separate QA velocity from DEV's
	- Plan QA iteration as for DEV
	- Track QA work as for DEV
- Define at least a DEV sprint
	- Drag stories into it
	- Sprint planning
	- Account for the demo when accepting story points
- Define at least a QA sprint
	- Do the same as for DEV

## Story Checklist

- Work only on the stories you've been assigned to in Jira, if you have doubts check with your lead.
- Understand requirements from the backend and the "parent" stories, if it's not clear ask your lead.
- Define interface with the UI in one or more meetings.
- Refine UI interface based on database and Genesys APIs.
- Write unit tests, TDD if you like.
- Develop incrementally, to help the UI team develop in parallel with you:
	- First provide the API to the UI with mocked values.
	- Then provide some real values.
	- Finally provide the full real values per the backend story requirements (could be joined to the above step).
- Definition of Done:
	- All non-boilerplate code covered by unit tests.
	- UI is able to consume your API.
	- Documentation updated, if any.
	- Automatic tests passed.
	- Sanity tests passed.
	- Commit & push in your branch successful.
	- Jenkins builds succeeded.
	- No increase in static analysis (e.g. Sonar) violations.
	- Code review passed.
	- Merged to master.
	- QA Test Cases reviewed.
	- QA work will be tracked in separate, QA, stories.  If they find issues they will raise bugs or we will raise other tasks or stories.

## App Architecture Checklist

When conceiving the architecture of an app, consider these:

- ( ) *Server type*: monolithic, micro-services / SCS, serverless.
- ( ) *Client type*: thin, fat.
- ( ) *AAA*: authentication, authorization, and accounting.
- ( ) *Features*: actual features you are building in your app.
- ( ) *Session Management*: create, maintain, distribute.
- ( ) *Security*: regulatory and best practices.
- ( ) *Automation tests*: unit, system, stress; add early.
- ( ) *Load*: target, start testing early.
- ( ) *Performance*: consider concurrency & parallelism.
- ( ) *Deployment*: target environments and methods.
- ( ) *Scalability*: vertical and horizontal.
- ( ) *High Availability*: take-over and recovery.
- ( ) *Monitoring & Logging*: quality logging, state & performance monitoring.
- ( ) *Documentation*: architecture, product, APIs, contracts.
- ( ) *Licensing*: impact of chosen 3rd-party items.
- ( ) *Infrastructure*: setups, deploy, and means to do them.
- ( ) *CI/CD*: desired results and means.
- ( ) *QA*: automated and manual, regression testing.
- ( ) *Team*: responsibilities, parallelism, process.
