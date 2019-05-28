1. Setup

a) This project was generated with yeoman:

# first install ASP.NET on the machine...

# install Yeoman...

npm install -g generator-aspnet

# create a src folder somewhere and cd to it...

yo aspnet

# choose Empty Application


b) To get all packages required by the project:

# cd to the folder that yo generated

dnvm upgrade

# this will set the default dnx to mono
# apparently on a Mac it doesn't work with coreclr

dnu restore


c) To build the app:

dnu build


d) To run the app:

dnx web

e) To install the DNX watcher:

dnu commands install Microsoft.Dnx.Watcher

f) To have the code changes being watched and the app automatically restarted:

dnx-watch web


2. Files

The initially generated files are:

- project.json: The project's configuration - dependencies, frameworks
- Startup.cs: The main app's file
- wwwroot/:
  - Readme.md
  - web.config
