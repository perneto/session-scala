#!/bin/bash

CLIENT1=10.0.1.1 # shade02
CLIENT2=10.0.2.1 # shadexx
CLIENT3=10.0.3.1 # shade04

MON1=10.0.1.254 # treebeard
MON2=10.0.2.254 # matrix16
MON3=10.0.3.254 # matrix13

BROKER=10.1.0.254 # shade07

. include.sh

start_monitor() {
  ssh -f $1 "echo $1: start monitor && cd $BENCHMARKS && hg up && ./start_monitor.sh"
}

start_client() {
  ssh $1 "echo $1: start $2 && . $BENCHMARKS/include.sh && start_role '$2' $3"
}


ssh -f $BROKER "if ! (rabbitmqctl status >/dev/null 2>&1) ; then rabbitmq-server; fi" > rabbitmq-server.out 
echo Started rabbitmq-server

start_monitor $MON1
start_monitor $MON2
start_monitor $MON3

start_client $CLIENT1 Inviter $MON1 &
start_client $CLIENT2 Buyer $MON2 &
start_client $CLIENT3 Seller $MON3 # not in background

ssh $BROKER "rabbitmqctl stop"
#ssh $MON1 "kill $"
