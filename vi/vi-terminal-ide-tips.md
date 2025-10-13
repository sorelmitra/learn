Load Simple Plugin / Script

Files in your .vim/plugin directory are sourced (loaded) automatically.


---


Search and Replace in a Directory Recursively

To perform a search and replace recursively in a directory in the UNIX way, use grep and vi/vim like this:

A) Make an alias
This will help avoiding long commands when you want to exclude certain things from grep.

	alias ge="grep -rnHEO --binary-files=without-match --exclude-dir=.git --exclude-dir=build --exclude-dir=Build --exclude-dir=coverage --exclude-dir=target --exclude-dir=dist --exclude-dir=node_modules --exclude-dir='.sst' --exclude='*.log' --exclude='*~' --exclude='*-lock.json' --exclude=tags $*"

B) Perform the search and check your file list

	ge -l 'foo' .

C) Perform the search again, telling vim to replace interactively on each file

	ge -l 'foo' . | xargs -L 1 -o vim -c '%s/foo/bar/gc'

Explanation of the command:

- The part till the pipe is the search command repeated
- After the pipe, we call `xargs` to launch vim on each file
	- `-L 1` tells `xargs` to launch the command on each line instead of combining all lines in a single command
	- `-o` tells `xargs` to use the actual terminal rather than `/dev/null`

If you prefer to launch vim on all files at once, remove `-L 1` from `xargs`.  This will launch vim on all files at once.
Advantages:
- Command is easy to cancel: just quit vim
Disadvantages:
- You have to switch to each buffer in turn and re-run the "%s" command on it
- If you have a lot of files, this can make vim consume a lot of memory (as much as a full-blown IDE!)

The other option is to launch vim in turn for each file by using the original command above.
Advantages:
- Everything is automated. Vim is launched and performs the search command
- Light on memory
Disadvantages:
- It's hard to cancel: quitting vim will just make xargs open the next one


---


Using ctags and cscope for Java

DIRS=". $JAVA_HOME/src $ANDROID_HOME/sources/android-17"
ctags -R --languages=+java $DIRS
find $DIRS -iname *.java > cscope.files
cscope -Rb
