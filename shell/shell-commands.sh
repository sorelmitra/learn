#!/bin/zsh

#
# Here we have various useful commands
#


# Run the same script with env vars that change their value on each execution:
# - ITEM cycles through ITEMS based on the value of "i"
# - GEN_AMOUNT generates values like 13.05, where "5" is math computed from "i"
COUNT=10 ITEMS=(an_item another_item yet_another_item); for (( i = 1; i <= $COUNT; i++ )); do; ITEM=$ITEMS[$(( ( i - 1 ) % 3 + 1 ))] && GEN_AMOUNT=`printf "%.2f\n" $(( ( 1300 + i ) / 100.0 ))` && env=dev && script=my-script.py && ENV=$env PM_ID=$ITEM AMOUNT=$GEN_AMOUNT python $script; echo "\n\n\n\n\n\n\n===\nDONE $i / $COUNT -- $env $ITEM $GEN_AMOUNT $script\n===\n\n\n\nSleeping a bit ..."; sleep 10; done; echo "\n\n\n\nDONE $COUNT runs.\n"


# Run the same command (in this case `npm run aat`) COUNT times.
# Count the failures and report them.
COUNT=5 FAILURES=0; for (( i = 1; i <= $COUNT; i++ )); do; npm run aat; res=$?; if [ $res != 0 ]; then; FAILURES=$((FAILURES+1)); fi; echo "\n\n\n\n\n\n\n===\nExecution number $i / $COUNT completed ($FAILURES failed)\n===\n\n\n\nSleeping a bit ..."; sleep 10; done; echo "Ran $COUNT tests, $((COUNT-FAILURES)) passed, $FAILURES failed."

# Run the same command (in this case `npm run aat`) COUNT times.
# Stop at first failure.
COUNT=5; STATUS="PASS"; for (( i = 1; i <= $COUNT; i++ )); do; npm run aat; res=$?; if [ $res != 0 ]; then; STATUS="FAIL"; fi; echo "\n\n\n\n\n\n\n===\nExecution number $i / $COUNT: $STATUS\n===\n\n\n"; if [ $res != 0 ]; then; i=$((i+1)); break; fi; echo "Sleeping a bit ..."; sleep 10; done; echo "Ran $((i-1)) / $COUNT tests: $STATUS."
