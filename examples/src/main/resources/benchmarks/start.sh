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
  ssh $1 "echo $1: $BENCHMARKS/start_monitor.sh"
}


ssh $BROKER "rabbitmq-server" > rabbitmq-server.out &
echo Started rabbitmq-server

start_monitor $MON1
start_monitor $MON2
start_monitor $MON3

ssh $CLIENT1 "echo $CLIENT1: cd $BENCHMARKS && . include.sh && start_role 'Inviter' $MON1"
ssh $CLIENT2 "echo $CLIENT2: cd $BENCHMARKS && . include.sh && start_role 'Buyer' $MON2"
ssh $CLIENT3 "echo $CLIENT3: cd $BENCHMARKS && . include.sh && start_role 'Seller' $MON3"

ssh $BROKER "rabbitmqctl stop"
