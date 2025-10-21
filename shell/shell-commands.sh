#!/bin/zsh

#
# Here we have various useful commands
#


# Run the same script with env vars that change their value on each execution:
# - ITEM cycles through ITEMS based on the value of "i"
# - GEN_AMOUNT generates values like 13.05, where "5" is math computed from "i"
COUNT=10 ITEMS=(an_item another_item yet_another_item); for (( i = 1; i <= $COUNT; i++ )); do; ITEM=$ITEMS[$(( ( i - 1 ) % 3 + 1 ))] && GEN_AMOUNT=`printf "%.2f\n" $(( ( 1300 + i ) / 100.0 ))` && env=dev && script=my-script.py && ENV=$env PM_ID=$ITEM AMOUNT=$GEN_AMOUNT python $script; echo "\n\n\n\n\n\n\n===\nDONE $i / $COUNT -- $env $ITEM $GEN_AMOUNT $script\n===\n\n\n\nSleeping a bit ..."; sleep 10; done; echo "\n\n\n\nDONE $COUNT runs.\n"
