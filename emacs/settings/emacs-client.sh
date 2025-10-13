#!/bin/sh

FILES=$*
OPTS=$FILES

if [ -z "$FILES" ] ; then
    OPTS="-c"
fi

/usr/local/bin/emacsclient -n $OPTS
if [[ $? = 0 ]] ; then
    exit
fi

/usr/local/bin/emacs --daemon
/usr/local/bin/emacsclient -n $OPTS
if [[ $? -ne 0 ]] ; then
    echo "I cannot start emacsclient!"
    exit
fi

