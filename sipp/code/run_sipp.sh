#!/bin/sh
/usr/bin/sipp 192.168.42.133 -sf scenario.xml -inf info.csv -rp 1s -i 192.168.42.133 -m 1 -nr -p 30188 -watchdog_major_maxtriggers 60 -aa -pause_msg_ign -trace_msg -trace_shortmsg -trace_err 
