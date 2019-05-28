#!/bin/sh

# DIRS=. $JAVA_HOME/src $ANDROID_HOME/sources/android-17
DIRS=.
ctags -R --languages=+java $DIRS
find $DIRS -iname *.java > cscope.files
cscope -Rb
