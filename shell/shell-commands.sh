#!/bin/zsh

#
# Here we have various useful commands
#


# Run the same script (e.g. flow_transaction_create_pay.py) with env vars
# that change their value:
# - PM_ID cycles through PMS based on the valu of "i"
# - AMOUNT generates values like 13.05, where "5" is math computed from "i"
COUNT=10 PMS=(pm_card_createDispute pm_card_createDisputeProductNotReceived pm_card_createDisputeInquiry); for (( i = 1; i <= $COUNT; i++ )); do; PM_ID=$PMS[$(( ( i - 1 ) % 3 + 1 ))] AMOUNT=`printf "%.2f\n" $(( ( 1300 + i ) / 100.0 ))` ENV=dev SCRIPT=~/1data/w/TQ/GL/gl-auto/pay/flows/transactions/flow_transaction_create_pay.py python $SCRIPT; echo "\n\n\n\n\n\n\n===\nDONE $ENV $PM_ID $AMOUNT $SCRIPT\n===\n\n\n\n"; done