# The Nine Node Pillars

9 Principles for Doing Node.js Right in Enterprise Environments
https://www.platformatichq.com/node-principles

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

# Challenge for Going into Production

As part of a coding interview I took a code challenge which gives you a code that accesses a Weather API.  This code is calling the basic API and has some entity definitions for the database, but it is not working with its database missing and some other problems, including some artificially introduced.  They tell you to make this code production-grade.

What I did instead was take the entire app and place it into production.  I did that as an exercise to see how fast and well can I do this.  So what I did was:

- Bring the code into a state where I believe it's manageable by me and my team.  This includes adding unit tests, fixing most of the issues, organizing the code so that it's unit testable.  It did not include things like adding a service or any further breakdown.  I based this on the principle "do the simplest thing that could possibly work", and that's because the unit testing supports further refactoring.  So once you get this code into production and it starts bringing you money, and you start building new features, you can easily and seamlessly do a refactoring to introduce services and any other structure you like.

- Add automated system tests.  They are usually a good idea at the backend level, more so if you don't have dedicated QA at this point - and this is likely with a startup.

- Add stress testing.  If you're gonna design an app that has the potential to grow to a large user base, it pays to consider load and stress testing early in the process.  This way you already have an infrastructure that will grow with your project, and when time comes to really assess the performance and scalability of your app, you'll have most of the tools already in place to do this.  As a bonus, there's a real chance you could catch some issues early, like I did with this project where I hit a limit of requests imposed by the free API.

- Add scalability.  For this, two approaches were used: First start using a thread pool to be able to serve multiple requests simultaneously with a single app instance, and this allows for vertical scaling.  Then, make sure the app is working in such a way that adding more replicas of it would not make it malfunction.  (For example in this case, two users could request the weather data for the same city at the same time, and only one of them will get the final value in the database.  But this is not foreseen as an issue since those values were requested at the same time and are likely to not change dramatically, and even if they do, that's not an issue because it probably means the API server has just updated the info for that city.)

- Add deployment.  Once the app is scalable, you can start thinking what environment to use to actually handle scalability for you.  I chose Kubernetes and thus wrote the necessary Helm files to support deployment.  At a very basic level, there's also plain Docker with Docker-Compose, but Kubernetes is the preferred choice if available.

- Consider high-availability.  Not part of this project, but I did think about how to make the app highly available.  I think the solution is at the infrastructure server, e.g. creating more clusters that stand by and are kicked in by the lab/cloud provider when the main cluster fails.  This also implies duplicating of the database to the new cluster, but I believe the main cloud providers already support this and if using an on-premise lab, it should be configured to do the same.

- Consider QA.  I forgot to consider this for this project, but you should at least ask yourself what you want to do.  Probably in the initial stages of a startup you'll have no QA and employ automatic tests instead.  But for later stages and if the product is likely to become more complex, QA must be taken into account and a decision made how to employ it.

This is a very simple, even simplistic, app that accesses a Weather API and stores the result in a database.  It is therefore a very good candidate to what I did above, because you can really focus on the work you need to do to put it into production, and you can get a grasp of the process.  I'm actually starting to believe it is worth putting the app into production from the beginning, as the process is more transparent and easier to manage at this stage.

All the above took me 3 days of hard, intense and under-pressure work, which I estimate to a total of 28 hours.  In a "normal situation", I'd expect to achieve the same in 5 working days.  And as a complexity measurement using story points, it would be an 8, which warrants breaking it down into two smaller stories, say a 3 points one for the code fixing and a 5 points one for the rest of the work.

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

