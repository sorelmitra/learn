# Agile Process Definitions

*Story*: Unit of work. Ideally small enough for a single engineer to do a few of these in one sprint.

*Point*: Story size measurement unit. It takes into account risks, complexity and repetition. It does NOT measure time.

Points scale: We use numbers from the Fibonacci series: 

- 1: Tiny story, the more, the merrier.
- 2: Very small story, you can have plenty of these.
- 3: Small story, still good to have.
- 5: Medium story, use if forced to.
- 8: Large story, avoid if possible.
- 13: Extra Large story. Avoid.  Make it an Epic.
- 21: Epic. Used mostly for initial estimation.
- 34: Epic. For initial estimation only.

*Sprint*: A fixed amount of time in which certain stories are planned to complete.

*Capacity*: The number of actual days an engineer can work in a given sprint.

*Velocity*: The number of points that the team can do per sprint.



# Estimating

If estimating for an agile sprint, then just set the points and you're good to go.

If estimating for an offer or to get a delivery date for a project, then follow these guidelines:

1. Keep it simple.  A plain TXT or Confluence table will do.  No need for automatic calculations, you're just estimating work at a high level.

2. Break down the work, with enough detail that you can estimate.  Ask yourself questions on how this thing will be implemented.

3. Consider the teams and their interdependencies, allocation, and skills.

4. Consider these factors:

	- Process.  This includes slow starting, communication, time spent in meetings.
	- Vacations.
	- QA.  It might start a bit later than DEV, and will depend on it.
	- DEV Automated testing.  Takes extra time from the DEV but it pays off with peace of mind.
	- Regression testing.  It takes time to be executed and bugs fixed.
	- Security checks.  Penetration test, code scan.  It takes time to be executed and issues resolved.
	- Licensing work.  For libraries that will be used in the code.  Could take up quite some time to fill in the paperwork and get the approval.

5. Add a buffer.

6. Review the overall estimation.  Find a good balance between having a decent work pace and securing the deal.



# Agile Work Tracking & Release Plan

Use Jira, an Agile Tracking software, or `agile-sprints.xslx`.

Create your sprints.  Assign backlog items into them with a decent velocity per sprint.  See the final date, add some more sprints for buffer.



# Non-Agile Work Tracking

The process below is the best I could come up if not using Agile.

## Description of `non-agile-work-tasks.xlsx`

The `ReqTracing` sheet contains the traceability matrix of requirements to work items. The rows contain Work Item Names and the columns contain IDs of requirements.
For the sample sheet, requirements could go like:

- CR01: Handle Call Recordings
- SEC01: Authorize users
- SEC02: Authenticate users

The `Overview` sheet contains all work items (e.g. features, documentation work). For each work item we have:

- Nr
- Work Item
- Description
- Tasks count (total, Dev, SV), computed from the Tasks sheet
- Remaining Team effort on that particular work item, in days, computed from the Tasks sheet
- Completeness percentage, computed from the Tasks sheet

The `Team` sheet shows when will each team/resource finish all their tasks.
For each team or resource, we have:

- Name
- Load per day = how much of a day the resource can work on this project; 1 is full-time, below 1 is part-time
- The remaining effort and duration
- The estimated end date starting from TODAY()
- The number of tasks not DONE
- Remaining vacations since TODAY()

The `Tasks` sheet contains all tasks for this work period.
For each task, we have:

- Status, only `DONE` is used in formulas
- Work item, referenced from `Overview`
- Task details
- Depends on: other tasks that this one depends on
- Team: allocated resource, referenced from `Team`
- Remaining effort, in days

## Process

1. Define the work items

2. Define the tasks

	* If using an issue-tracking mechanism

		- Assign quick temp IDs to each task and put the task description in the Notes column
		- Assign each task to the resource
		- Once all tasks are assigned and described, you will have an overview of all the tasks
		- You can now create issues in your tracking software and replace ALL OCCURENCES of a task temp ID with the issue-tracking ID

	* If noting tasks locally

		- Assign unique IDs to each task and put the task with ID and description in a `tasks.txt` file
		- Assign each task to the team/resource
		- Once all tasks are assigned and described, you will have an overview of all the tasks

3. Estimate tasks in the `non-agile-work-tasks.xlsx` document

	* Put initial estimate in the "Remaining effort" column
	* Put the target date in the Team sheet
	* Put the known vacations in the Team sheet

4. Baseline the Excel document by copying it to `non-agile-work-tasks-baseline1.xlsx`

	- Then, only the remaining work is tracked (because for me tracking the past work is impossible):
	- Initially or during status meetings, tasks are assigned to resources.

5. You can now start watching and updating status in the `non-agile-work-tasks.xlsx` document. You can do this as often as daily, or at bigger intervals if you wish. Whenever you update the status, do the following:

	* For each task that's STARTED, ask the allocated resource how much effort it remains for them on this task since TODAY
	* Update the provided remaining effort in the Tasks sheet
	* When a task is DONE, remaining effort is set to 0
	* Ask each resource about foreseen vacations
	* Put in the Team sheet the remaining vacations that happen before the target date 
	* In the Team sheet watch the projected date against the target date

6. At each status meeting:

	- Resources are asked to provide remaining effort for each task they have in work. This is put in the corresponding column
	- When a task is done or only has reviews pending, remaining effort is set to 0
	- For each resource, the remaining vacation starting from TODAY() is updated in the corresponding column

7. At the end of the Sprint/period, the remaining effort is 0 for all tasks that were done.

8. When a new Sprint starts, the tasks with effort 0 are deleted. The Tasks sheet is filled in with the new tasks.

Advantages:

It's really easy to track remaining work and end dates. If you forget to update remaining effort or vacation for a resource, the Summary status will push the end date later. So you're forced to have an accurate remaining effort, or the end dates will look bad.

Disadvantages:

You cannot track total effort spent on each task.
Not that I think I can ever do that without an automatic and enforced work log system.

## Description of `non-agile-work-velocity.xlsx`

For each Sprint, you note its name, period, and issues count.
Then you put each week in a row.

At the end of each week you:
- Update the Sprint Total issues count
- Note down the count of solved issues. Leave blank for the weeks not yet passed

Following that is a section that computes for you:
- Count of solved issues
- Count of passed weeks
- Current velocity
- Count of remaining issues and weeks
- Velocity needed to finish all issues on time
- Count of issues that don't fit the sprint based on the current and needed velocity
