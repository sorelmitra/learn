# Automation

## Be Pragmatic with Automation

1. never again do full automatic system tests:
	* in a complex system that requires puting lots of pieces together (e.g. REST APIs, queues, emails, webhooks, etc.)
	* that require complex local setups
	* off the record in a team that doesn't already have them
2. always do simple automation:
	* full automatic system tests with a simple system (e.g. just REST API calls and response validation)
	* simple scripts that use the `hol` library to automate certain actions, accompanied by a rigorous test plan that you follow, for more complex system
	* consider automating tedious UI forms that you have to do repeatedly
	* even off the record if the team doesn't adhere to automation
3. keep your unit tests simple and exclude from coverage files that cannot be tested in a simple manner
4. in a startup situation:
	* start with manual tests
	* add simple automation or automatic system tests later, when the product is beginning to gain shape
	* use unit tests wisely: they provide a lot of value for verifying and nailing down code logic, and they also can cover you up when fully automatic system testing is not practicable; just make sure you don't enter mocking hell
4. in a team that follows a testing process that doesn't suit your needs:
	* follow their best practices, stay in line with the team
	* assist yourself with the ideas outlined above

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

# QA Work Estimation and Tracking

Source: https://link.springer.com/chapter/10.1007/978-3-540-68255-4_32

Many times in a software development project, the following questions arise:
Can we track QA (Quality Assurance) work as we do with development? In another word, can we capture all the QA activities and work in a backlog list and do the iteration and release planning based on the QA staff plan, and assumed QA velocity? Can we also generate QA status report such as the burn up or burn down chart?

Before answering these questions, a short analysis: No doubt, QA is a core part for an Agile project. Suppose developers achieve the throughput per iteration as scheduled, does it mean the project will be released as planned? Not necessarily, what if QA could not complete testing all stories from previous dev iterations? What if QA has to support interim release testing? What if QA is pulled into other activities than iteration testing? What if QA team is mixed of on shore and offshore members, and has to support multiple dev teams at the same time? All these questions become the reasons behind the very first one – "Can we track QA as we do with our Dev?"

Case study: The first project I worked on was for an investment banking client who had a QA team of 6 people with 4 on shore and 2 off shore, supporting testing for 5 development teams of almost 30 developers. The team faced several issues: There was no QA estimation for story; QA spent lots of time on non-story activities without tracking; the ratio QA to developers was 1:5.  The outcome wasn't good.

Case study 2: The second team I worked with was a QA team of an e-commerce retailer IT department. The team had 3 people supporting 6 developers. The team mixed the dev velocity with QA's. By their definition, velocity was the amount of work that passed UAT in an iteration, measured in dev estimation points, while at the same time, Iteration planning only took dev velocity into consideration. The QA estimation for each story was well off mark. And as same as the first team, QA team members were pulled regularly to support release testing and other non project related activities.

Solution: We helped both teams by adopting similar tracking mechanism we used for the dev team:

- We first created a complete QA backlog list including all the stories, plus the non-story QA tasks.
- In the backlog list, we indicated which dev iteration and QA iteration each story was scheduled in.
- We then did QA estimation for each story using the “triangulation” rule.
- We separated QA velocity from Dev's.
- We planned QA iteration as we did for Dev, and provided QA status report and burn up chart per iteration.

Both projects achieved good results:

- We better managed QA work load – By having a complete QA backlog list and giving QA estimation for each story, the team had a clear idea about QA scope.

- We better managed QA non story activities – By having QA non-story activities in the QA backlog list, and having them prioritized along with the stories, QA team could schedule these activities along with stories in each iteration. Thus QA team could work on these technical debts without being unnoticed.

- We better tracked QA velocity and load factor – Tracking QA team velocity and load factor separately helped the team with QA iteration planning, velocity measuring and monitoring and team status reporting.

- We better managed QA team staffing – Since now the QA team had the complete scope measured in points, the team velocity, the project release plan and date, it served better for QA to figure out how many people the team needed in order to finish all the testing to make sure the project could be released on time.

- We clarified the definition of team velocity – “Never mix the QA velocity with team velocity”. This is the lesson we learnt. Using QA velocity as team velocity could mislead the business users and mis-communicate the team status to the upper management. For example, the burn up chart shows the total amount of work in dev estimation points, the dev velocity per iteration, expressed in story points and the trend line which predicts the possible completion date based on the team velocity so far. If the team velocity is defined as the total points of stories only signed off by QA, it means even the stories are completed, but they are not credited if QA hasn't signed off yet. In another word, the team velocity is more QA velocity rather than Dev's, while the scope line still represents the total dev estimation points. Obviously, you will immediately realize these two are not incommensurable. This can also sets wrong expectation for business users who would expect the dev team velocity instead of the QA's.

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

