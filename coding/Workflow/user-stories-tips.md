# User Stories Info

From a series of 3 free online video seminars held by Mike Cohn (http://www.betteruserstories.com/training/videos/1)

Sorel's ideas & questions added here and there (not marked in the document...).

# Story Size

Optimum story size: <= 1/4 team's velocity?

# User Stories Workshop

## Purpose

The **purpose** of the Workshop is to understand **what users do** with the system and how they use it, and to **capture this** in User Stories that the Development Team can work on in iterations.

Prioritising the stories is **not** the purpose of such an workshop.

## Acronyms

- MVP: Minimal Viable Product

- MMF: Minimum Marketable Feature (from the "Software by Numbers" book)

## Workshop Tips

### Involve the Right People at the Right Times

You need to involve the following people in creating user stories:

- Product Owner

- Development Team

- Product Users

User stories are NOT a replacement of a requirements document.
We dont tear up a big document full of "The system shall do this" or "The system shall do that" with an equally long document full of "As a user, I want this" or "As a user, I want that"

So instead of a long initial user stories session, it's better to do several sessions throughout a project's life.

### Have a Single Properly Sized Big Goal of this Workshop

Key points:

1. Product Owner chooses a single Big Goal for this workshop

	- This goal can be an MVP in the beginning or an MMF once the viability of the product has been established

	- Product Owner needs to talk to the stakeholder and users to decide the Goal to propose for this workshop

2. The workshop should **not** be done at each iteration, so the Big Goal should fit in 2-3 iterations

	- A good recurrence of these workshops is quarterly

### Have the Right Participants

It's important to include the key participants in this workshop:

1. Moderator (Scrum master, Coach, etc)

	- This role is very important to keep the workshop on track, to avoid disruptions, or even to propose a break

2. Development Team: Developers, Testers, Designers, Artists, etc.

	- Including the development team:
	
		- Makes them more involved later
		
		- Saves time later because questions and issues are rised very early
	
		- Stimulates creativity by obtaining ideas from the team
		
	- If we have a large team or multiple teams:
	
		- We could include only part of the team, selecting participants based on who's more likely to contribute most and who is willing to participate
	
		- Alternatively, we could split the big goal into items that each team is responsible for identifying stories on. This would mean one smaller workshop for each item. Still, the whole team would participate in each smaller workshop

3. If possible: Stakeholders and Users

	- If stakeholders and users start arguing about which feature is more important, a good way to stop or prevent this is to make them understand the purpose of the workshop

### Use a Story Map

A story map arranges stories on two dimensions in order to help with organising an prioritising them.

The first dimension, horizontal, lists the stories in the logical order based on how the users use the system.

The second dimension, vertical, has two purposes:

1. Allows showing alternative ways of doing the same action (such as creating a report from scratch or copying an existing one)

2. Allows for arranging and grouping stories based on priority. We can even draw horizontal lines that mark milestones, and label them appropriately

The Story Map could also show other items, such as:

- Releases, separated with horizontal lines and labeled

- Story dependancies and risks with coloured sticky notes

Example:

	(1) Log in    Start new        Enter expenses    Attach receipts    Submit report
	              expense report   
    
	(2)           Create report                      Scan a receipt
	              from scratch               
				  
	(3)			  Copy existing                      Attach existing
				  report                             PDF or image

In the above example, on line (1) we have the logical flow of actions.
On line (2) and (3) we have the alternative ways of doing certain things.

Below we have a story map with the above stories prioritised and placed in releases.
(The horizontal line is placed **after** the stories included in that release.)


	(1) Log in    Start new        Enter expenses    Attach receipts    Submit report
	              expense report   
    
	(2)           Create report                      Attach existing
	              from scratch                       PDF or image
	
	[Initial Release]
	-----------------------------------------------------------------------------------
	
	(3)			  Copy existing                      
				  report                             
	
	[Needed Soon]
	-----------------------------------------------------------------------------------
	
	(4)			                                     Scan a receipt
				  
	[Nice to Have]
	-----------------------------------------------------------------------------------
	


# Splitting Stories


## Purpose of Splitting Stories

Splitting stories help with:

1. Giving a sense of satisfaction and motivation on the team to finish them, if they're small enough to fit a single iteration

2. Having a predictable velocity

3. Helps with showing progress

4. What else?

## Difficulties when Splitting

Common challenges when splitting stories:

1. How to split stories in such a way that shows progress, but also can be done in one iteration

2. How to spend a reasonable amount of time splitting stories without doing at the cost of getting something built

3. How to avoid splitting stories between development activities, such as programming and testing

4. How to remember and apply 50 ways of splitting stories, as you find in common web sites

## Story Types when Splitting

NOT every story can actually be split. There is a category of stories that cannot be split. They are called "Complex Stories", and are:

- Fundamentally large

- Cannot be split

- Rare

Most stories, however, are splittable, and they are called "Compound Stories". They:

- Comprise multiple smaller stories

- Can be split, although sometimes this can be hard

## Story Splitting Tips

1. Use the SPIDR approaches

2. Have only part of the team working on splitting to reduce time spent on this

	- If they use the SPIDR approaches, they can be really good at this

## SPIDR Splitting Approaches

The SPIDR way of splitting stories is comprised of five approaches, each abbreviated by a letter of a word that names the approach.

These approaches are not mutually exclusive, and you do **not** want to use them all in order to find the best way of splitting the story.
The rule of thumb is:

Start with whichever approach you like or feel could apply and use it. If it gives you a good split, then you're done. If not, look at another one, and so on, leaving Spike the last one.

The SPIDR approaches are outlined below.

### Spike

A "Spike" is a research activity intended to build knowledge about that story in the team. When the team becomes "smarter" about a story, sometimes that story becomes smaller.

Use Spike when:

1. All other approaches from SPIDR failed to help splitting it

	- It could be easy to abuse doing Spikes, and it is a time-consuming approach, so make sure you use it only after all other approaches failed

2. The team doesn't know how to implement it - that probably makes the story large

3. The team needs to know a new technology prior to implementing it - that, too can make a story look large

4. The story has multiple ways to implement it, and the team cannot decide which one to use until experimenting

To conduct a Spike, have the team (or part of it) do the necessary work, depending on the case:

- Research

- Learning

- Experimenting

This work can be done in one iteration. The results will be that the story will become smaller and easier to split and implement later in another iteration.

### Paths

The "Paths" approach investigates if a story can have multiple paths and steps to achieve it. If this is true, then one or more paths or steps could comprise a separate story.

In this approach, you:

1. Draw simple flow diagrams

2. Identify paths that are big enough to be worth removing from the original story and make their own stories

3. Identify steps that are the same

For example, if a story says that a customer can pay with a credit card or with PayPal, the diagram could be:

	
             Put Items in Cart
                     |
                    Pay
                     |
            ------------------
	        |                |
    With Credit Card    With PayPal
	        |                |
            ------------------
                     |
                Confirm Order
        

Here, we could have two stories: "As a customer, I can pay with a credit card", and "As a customer, I can pay with PayPal".

But if you draw your flow diagram like this:

	
                    Put Items in Cart
                           |
                          Pay
                           |
            ------------------------------
	        |              |             |
    With MasterCard    With Visa    With PayPal
	        |              |             |
            ------------------------------
                           |
                       Confirm Order
		

In the above diagram, you might reason that it's better to not put MasterCard and Visa in separate stories, but rather have the same two stories suggested at the first diagram.

Sometimes when drawing diagrams like above, you might identify a step that is big enough to worth to be removed from the original story.

For example, the "Confirm Order" step could have many features that make it big enough, such as multiple addresses, address suggestions and completions, age checks, etc.

In these cases, it is worth to remove that step from the original story

The goal of the "Paths" approach is to:

1. Have as few split stories as posible

	- You don't want to put each path or step into its own story

2. Have the split stories fit comfortably in a single iteration

	- You want small enough stories that you can make in an iteration

3. Identify paths or steps that:

	- Will deliver value to the Customers, Stakeholders, etc.
	
	- Are big enough that removing them from the original story will make that story smaller


### Interfaces

This approach splits based on the various Interfaces that your program can have.

For User Interfaces you could split stories:

- Based on the User Platforms (for example Desktop/Mobile), or User Agents (such as different browsers). Sometimes identifying which platform or user agent is causing trouble could lead to creating just two stories: one for the "Troublesome Thing", and one for the "Rest of Things".

- Developing an initial UI that has all the elements connected to a backend, but in a very simple and un-designed format. Then in other iterations add design incrementally

For Data Interfaces, you could split stories based on Data Format.

For example, if you need to import data from a file with possible formats Excel, XML, CSV, you could split in three stories:

- Two stories will be dependent on the first one to be implemented

- That first story will need to implement the data layer

- Subsequent stories would parse their format and just call the data layer

For Programming Interfaces, we could have 

### Data

When a story is large because it has to use a lot of Data, we can split it into multiple stories by having the app support subsets of that data incrementally.

For example, if you make a software for movie companies, one story could state "I want to know the best date on which to release or new movie". Since this takes into account variables like movie type, release early/later, release per regions, release date of other movies, etc. In this case each item of the above list could create a separate story. Even if an iteration release cannot be used to actually plan release dates, it still provides a step towards that goal.

### Rules

Relax business rules or technology standards in an initial version of a story, then work incrementally towards supporting the full rules or standards.

These rules could be: business rules, industry standards, performance requirements, etc.

The initial release of the story will be fully functional, but will not meet those rules. Then we will have other stories that specify compliance to some or all of those rules, and those stories will be prioritised in the upcoming iterations.


# Story Details

## The Problem

If you put too little detail in a story, you could end up delivering wrong functionality and spending time fixing that later in the iterations.

- The team could spend time waiting for answers

- Or discover that the details you get increase the size of the story

- Or that they simply understood some of it wrong in the first place

If you put too much detail in a story, you could have delays in delivering functionality or having the team bogged down or overly constrained in the implementation.

- Because the team tries to solve every issue in a story, they spend a lot of time doing this, and even if they implement stories fast, the time spent in fixing all the issues before starting implementation has a negative impact on the delivery date

## The Solution

In the retrospective meeting, ask the team on the past stories "Did you get just enough detail to finish this story in this iteration, just in time?"

"Just enough detail":

- For each story, there are **some** details left to be worked out with the Product Owner. "Just enough detail" means that if one more detail had been left to work out during the iteration, then the team couldn't finish the story

- If too many details were added to the story, the team started feeling like "Code Monkeys" that have no creativity allowed

- You do not need to get every story with perfect detail - some of them will have not enough detail, some a little too much. You need to get a good average of details on the stories

"Just in time":

- If provided too early, it could be out of date and need rework when implementation starts

- If provided too late, it could cause the story to not be finished in the iteration

Based on team's answers, work with your Product Owner and the team to aim that for future iterations you are more and more closer to the goal that the details are provided "just enough", and "just in time".

Make the team feel comfortably with missing details sometimes: they should understand that failing now-and-then to provide details "just enough", and "just in time" is acceptable as long as on the average this happens.

As an example, FedEx sometimes delivers 1-2 days later, but usually they respect their promise of 1-2 days delivery. If they were to promise 0% delays, they'd either need to advertise 1 week instead of 1-2 days, either double their resources - neither is a good option. The same applies to the Product Owner delivering the details to the team.
