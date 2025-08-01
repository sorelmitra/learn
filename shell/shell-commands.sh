#!/bin/zsh

#
# Here we have various useful commands
#


# Run the same script (e.g. flow_transaction_create_pay.py) with env vars
# that change their value:
# - PM_ID cycles through PMS based on the valu of "i"
# - AMOUNT generates values like 13.05, where "5" is math computed from "i"
COUNT=10 PMS=(pm_card_createDispute pm_card_createDisputeProductNotReceived pm_card_createDisputeInquiry); for (( i = 1; i <= $COUNT; i++ )); do; pm_id=$PMS[$(( ( i - 1 ) % 3 + 1 ))] && amount=`printf "%.2f\n" $(( ( 1300 + i ) / 100.0 ))` && env=dev && script=~/1data/w/TQ/GL/gl-auto/pay/flows/transactions/flow_transaction_create_pay.py && ENV=$env PM_ID=$pm_id AMOUNT=$amount python $script; echo "\n\n\n\n\n\n\n===\nDONE $i / $COUNT -- $env $pm_id $amount $script\n===\n\n\n\nSleeping a bit ..."; sleep 10; done; echo "\n\n\n\nDONE $COUNT runs.\n"
