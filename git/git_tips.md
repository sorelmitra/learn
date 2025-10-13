# Save GitHub PR as Patch

Append `.patch` to PR's URL.  E.g., from:
https://github.com/php/php-src/pull/296

To:
https://github.com/php/php-src/pull/296.patch

This will open the patch form of that PR.



# Configuring Email per Repo

First, when cloning do something like this:

For the work account:

	P="authorization-service" && git clone git@github.com-sorelmitra-goodleap:loanpal-sawbridge/${P}.git && cd ${P} && git config user.email "smitra@goodleap.com"

For the personal account

	P="learn" && git clone git@github.com:sorelmitra/${P}.git && cd ${P} && git config user.email "sorelmitra@yahoo.com"

Second, if you have some repos already, you can find out what email they've configured:

	find . -type d -maxdepth 1 -not -name '.' -exec sh -c "echo '\n{}'; cd '{}'; git config user.email; cd ~-;" \;

Or change email for all of them to `a@b.com`:

	find . -type d -maxdepth 1 -not -name '.' -exec sh -c "echo '\n{}'; cd '{}'; git config user.email a@b.com; cd ~-;" \;



# Use Multiple GitHub accounts on the same computer

A) Generate an SSH-key:

	ssh-keygen -t ed25519 -C "john@doe.example.com"

B) Follow the prompts and decide a name, e.g. `id_ed25519_example_company`.
Copy the SSH public-key to GitHub from `~/.ssh/id_ed25519_doe_company.pub` and tell `ssh` about the key:

	ssh-add ~/.ssh/id_ed25519_doe_company

C) Create a config file in `~/.ssh` with the following contents:

	Host github-doe-company
	  HostName github.com
	  User git
	  IdentityFile ~/.ssh/id_ed25519_doe_company

D) Add the public key to GitHub -> Settings -> SSH and PGP keys -> New Key -> Authentication Key

E) Test the connection

	ssh -T git@github.com

https://stackoverflow.com/a/3860139

http://code.tutsplus.com/tutorials/quick-tip-how-to-work-with-github-and-multiple-accounts--net-22574



# Operate using SSH keys instead of passwords

https://phoenixnap.com/kb/git-clone-ssh



# Disable Pager

Do:

	git config --global core.pager cat

to get rid of a pager for all commands for all repositories.


# Don't Do Squash Merges

In general, there are a variety of problems that can occur when using squash merges to merge two branches multiple times. These can include seeing extra commits in git log output, with a GUI, or when using the ... notation to express a range, as well as the possibility of needing to re-resolve conflicts again and again.

When Git does a normal merge between two branches, it considers exactly three points: the two branches and a third commit, called the merge base, which is usually the common ancestor of the commits. The result of the merge is the sum of the changes between the merge base and each head. When you merge two branches with a regular merge commit, this results in a new commit which will end up as a merge base when they’re merged again, because there is now a new common ancestor. Git doesn’t have to consider changes that occurred before the merge base, so you don’t have to re-resolve any conflicts you resolved before.

When you perform a squash merge, a merge commit isn’t created; instead, the changes from one side are applied as a regular commit to the other side. This means that the merge base for these branches won’t have changed, and so when Git goes to perform its next merge, it considers all of the changes that it considered the last time plus the new changes. That means any conflicts may need to be re-resolved. Similarly, anything using the ... notation in git diff, git log, or a GUI will result in showing all of the changes since the original merge base.
This approach is always going to lead to problems. If you have two long-running branches, you need to use a regular merge commit. Otherwise, you could create short-lived branches that you merge into each of your long running branches separately, or use a rebasing strategy. All of these approaches are more appealing if you take the time to create independent, logical commits with good commit message like Git encourages you to do.



# Alternative to Merge Hell

Say you want to bring branch `my_feature` in sync with `main`, but you haven't done this in a while and now both merge and rebase give you a ton of ugly conflicts in someone else's code that you never touched in your branch.

Since you know your code better than other people's code, a safer option might be to just port your changes from your branch into a new branch freshly taken from the base one.

Here's how to do it:

1. Make sure all unit and auto tests offer good coverage on your code.
2. Make sure all tests in the `my_feature` branch pass.
3. Update `main`, and branch `my_feature_port` from it.
4. Run all tests: some of them will definitely fail.
5. Find the most recent base for your branch: `git merge-base main my_feature`.  It will output a commit SHA, say `XXX`.
6. Run `git diff my_feature XXX > ~/Downloads/my_feature.diff`, it will contain a subset of your changes since the most recent merge you did from `main` into `my_feature`.
7. Port changes into `my_feature_port` based on this diff.  Vim could be a good tool with its `zf` folding to next `/^diff`, it's `gF` based on `VISUAL` selection, macro capabilities, and `:mkview` plus `:loadview` to save folds.
	- For a small number of changes (whether large or small edits): port them by hand.
	- For a large number of changes: use `git log -p` on the file to see when it was last modified.  If you're lucky, and this happened before your changes, you can simply copy the file from `my_feature` to `my_feature_port`.
8. Do any required fixes.
9. Run tests:
	- Pass? Done.
	- Fail? Go to to step 7.



# Porting Changes under Feature Flags

1. Follow the steps `1..5` from the `Alternative to Merge Hell` above.
2. Generate diff with `git diff -W my_feature XXX > ~/Downloads/my_feature.diff`
	- Using `-W` shows the whole function context, and it helps when you need to port chunks of code that are gonna be wrapped in feature flags.
3. Find a non-unit test file.
4. Find a function with some changes to port.
5. Are the changes isolated into a particular portion of that function?
	- Yes: Identify a portion of that function for wrapping into the feature flag.
	- No: Wrap the whole function into the feature flag.
6. If you find deleted-only code, don't delete it.
	- If it was moved elsewhere, add it where it was moved, under the feature flag, throwing exception on the else branch.
7. Add the code that you identified under the feature flag, on the false branch.
8. Add your ported code on the true branch of the feature flag.
	- Along with the functions it depends on in this or other files.  Apply the same procedure recursively...
9. Fix unit tests corresponding to the code you changed:
	- Existing unit tests from `main` should mock that feature flag to OFF.
	- Add unit tests from `my_feature` and make them mock the feature flag to ON.
10. More functions with changes to port in that file?
	- Yes: Go to step 4.
	- No: Continue.
11. More non-unit test files?
	- Yes: Go to step 3.
	- No: Continue.
12. Run tests:
	- Pass? Done.
	- Fail? Go to to step 7.




# Merge vs Rebase

## Merge

Let's say you have created a branch for the purpose of developing a single feature. When you want to bring those changes back to master, you probably want to do a squash merge (you don't care about maintaining all of the interim commits).

You can also do a `merge --squash` to create a cleaner PR, i.e. one that does not have a ton of commit messages.  See below.

## Avoid Rebase Hell: `merge --squash`

Suppose you have a feature branch named `my_feature`.  You've been working on this for a while and kept on sync-ing the `master` branch back into your branch.  So now your PR would look like this:

	7e181479 Adds methods for widget sales
	0487162 [merge] Merge remote-tracking branch 'origin/master' into my_feature
	76ee81c [merge] Merge branch 'my_feature' of https://github.com/my_user_name/widgets into my_feature
	981aab4 Adds api for the widget service.
	b048836 Includes fixes suggested by reviewer.
	3dd0c22 adds changes requested by reviewer
	5891db2 [merge] fixing merge conflicts
	2e226e4 fixes suggestions given by the reviewer
	da1e85c Adds gadget related API spec
	c555cc1 Adds doodad related API spec
	e5beb3e Adds api for update and delete of widgets
	c43bade Adds api for creating widgets
	deaa962 Adds all get methods for listing widgets
	9de79ab Adds api for showing a widget and simple data model
	8288ab1 Adds api framework for widget service

So you want a clean PR:  In this method, you will create a temporary branch and use `git merge --squash` to squash together the changes in your pull request.

1. Check out a new branch based on `master` (or the appropriate base branch if your feature branch isn’t based on `master`):

		git checkout -b work master

	This creates a new branch called work and makes that your current branch.

2. Bring in the changes from your messy pull request using git merge --squash:

		git merge --squash my_feature

	This brings in all the changes from your my_feature branch and stages them, but does not create any commits.

3. Commit the changes with an appropriate commit message:

		git commit -m "API for widgets"

	At this point, your work branch should be identical to the original `my_feature` branch (running `git diff my_feature` should not show any changes), but it will have only a single commit after master.

4. Return to your feature branch and reset it to the squashed version:

		git checkout my_feature
		git reset --hard work

5. Update your pull request:

		git push -f

6. Optionally clean up your work branch:

		git branch -D work


## Rebase

A second scenario would be if you started doing some development and then another developer made an unrelated change in the parent branch. You probably want to pull and then rebase to base your changes from the current version from the repository.

A rebase might generate the same conflict multiple times and requires you to pull and push several times until the tree can be moved safely forward.  It is a bit more complicated than a merge, but it pays later when you want to merge back to the parent branch.

[Later update] Except that rebase ends up in a ton of meaningless conflicts that can lead you into broken code updates and mess up everything... it happened to me!

If you were to do a merge for this scenario, it might be simpler now but harder later when someone else wants to merge your branch back to the parent.

See diagram below.

Branch x with some changes (y), created from dev.  And changes (z) pushed to dev by someone else.

dev
|---x
|   |
|   (y)
|
(z)
|
|



Merge: brings (z) into x

dev
|---x
|   |
|   (y)
|   (z)
|
(z)
|
|



Rebase: moves x forward without bringing (z) into it

dev
|
(z)
|
|---x
|   |
|   (y)
|



# Cleanup Branches

Local pushed branches:

	git branch | grep -vE '(master)|(main)|(develop)' | xargs -n 1 git branch -d

Remote branches:

	git branch -a | grep smitra | awk -F '/' '{print $3 "/" $4}' | xargs -n 1 git push origin --delete


# See the tree of the branches

## Show branches (including remote) and tags 

	git log --graph --simplify-by-decoration --pretty=format:'%d' --all

This does a pretty good job. It shows tags and remote branches as well. This may not be desirable for everyone, but I find it useful. --simplifiy-by-decoration is the big trick here for limiting the refs shown.

## Show commits and branches, variant 1

	git log --graph --pretty=format:'%C(yellow)%h%Creset%C(blue)%d%Creset %C(white bold)%s%Creset %C(white dim)(by %an %ar)%Creset' --all

## Show commits and branches, variant 2

	git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset%n' --abbrev-commit --date=relative --branches

I use it by including these aliases in my ~/.gitconfig file:

	[alias]
	    # Show branches (including remote) and tags 
	    l = log --graph --simplify-by-decoration --pretty=format:'%d' --all

	    # Show commits and branches, variant 1
	    ll = log --graph --pretty=format:'%C(red)%h%Creset%C(yellow)%d%Creset %s %C(green)(by %an %ar)%Creset' --all

	    # Show commits and branches, variant 2
	    ll2 = log --graph --pretty=format:'%C(red)%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset%n' --abbrev-commit --date=relative --branches



# Operate on multiple directories

	find . -type d -maxdepth 1 -not -name '\.*' -exec bash -c '. ~/.bash_profile && echo && cd {} && echo -e "===\033[0;35m{}\033[0m \033[0;34m$(parse_git_branch)\033[0m===" && git status -s && cd $OLDPWD' \;



# Merging Two Git Repositories Into One Repository Without Losing File History

The basic idea is that we follow these steps:

1. Create a new empty repository New.
2. Make an initial commit because we need one before we do a merge.
3. Add a remote to old repository OldA.
4. Merge OldA/master to New/master.
5. Make a subdirectory OldA.
6. Move all files into subdirectory OldA.
7. Commit all of the file moves.
8. Repeat 3-6 for OldB.

It would be like this:

	# Assume the current directory is where we want the new repository to be created
	# Create the new repository
	git init

	# Before we do a merge, we have to have an initial commit, so we’ll make a dummy commit
	dir > deleteme.txt
	git add .
	git commit -m “Initial dummy commit”

	# Add a remote for and fetch the old repo
	git remote add -f old_a <OldA repo URL>

	# Merge the files from old_a/master into new/master
	git merge old_a/master

	# Clean up our dummy file because we don’t need it any more
	git rm .\deleteme.txt
	git commit -m “Clean up initial file”

	# Move the old_a repo files and folders into a subdirectory so they don’t collide with the other repo coming later
	mkdir old_a
	dir –exclude old_a | %{git mv $_.Name old_a}

	# Commit the move
	git commit -m “Move old_a files into subdir”

	# Do the same thing for old_b
	git remote add -f old_b <OldB repo URL>
	git merge old_b/master
	mkdir old_b
	dir –exclude old_a,old_b | %{git mv $_.Name old_b}
	git commit -m “Move old_b files into subdir”

