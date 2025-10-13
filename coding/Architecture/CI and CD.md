# CI/CD

A growing number of companies are shipping software in minutes.  Yeah, you read that right. Minutes. Not hours, not weeks, months, or longer. Minutes.

Often, teams struggle to ship software into the customer's hands due to lack of consistency and excessive manual labor. Continuous integration (CI) and continuous delivery (CD) deliver software to a production environment with speed, safety, and reliability.

## CI

Continuous Integration (CI) is a development practice that requires developers to integrate code into a shared repository several times a day. Each check-in is then verified by an automated build, allowing teams to detect problems early. By integrating regularly, you can detect errors quickly, and locate them more easily.

If you're doing CI and for some reason the integration fails, that means the broken build becomes the highest priority to fix before continuing to add more features. System quality—not just velocity—is important. CI works in three simple stages: push, test, and fix. But despite this simplicity, CI might become challenging if only a few members of the team practice it. Consequently, CI also requires a change in culture and support from management.

Fixing the build, as a principle, comes from the Toyota andon cord that the car manufacturer used to produce cars. [John Willis wrote a post on the subject](https://itrevolution.com/kata/), and in it, he describes Toyota's process like this:

Toyota implemented the Andon Cord as a physical rope that followed the assembly line and could be pulled to stop the manufacturing line at any time. Furthermore, this wasn't an ask permission to stop the line. The pull actually stopped the line.

Lack of tools is no excuse for not doing CI, as [this hilarious post](https://www.jamesshore.com/Blog/Continuous-Integration-on-a-Dollar-a-Day.html) outlines.

## CD

Continuous Delivery is the ability to get changes of all types—including new features, configuration changes, bug fixes and experiments—into production, or into the hands of users, safely and quickly in a sustainable way. We achieve all this by ensuring our code is always in a deployable state, even in the face of teams of thousands of developers making changes on a daily basis.

Usually, CI is known to be a developer's practice and CD an operator's practice. CI's mission is to provide an artifact at some point in time of the application that satisfies customer expectations—in other words, that has good quality built in.

CI's mission is then to move those artifacts throughout all the different environments of an organization's development lifecycle. What's critical in CD is that it will always deploy the same artifact in all environments. Therefore, a build in CI happens only once and not for each environment. The artifact produced will work with placeholders or environment variables for the build-once approach to work.

CD principles:

- Frequent small deployments, "if it hurts, do it more often."
- Automation with a human touch: automate but rely on people to improve the process.
- Always improving.  Measure first, to know what to improve.
- Shared responsibility model - operations will want to help, rather than point at, developers.  And viceversa.

- https://stackify.com/what-is-cicd-whats-important-and-how-to-get-it-right/

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

