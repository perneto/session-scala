#!/bin/bash

source include.sh

#(cd SessML; svn up && make)


nc -l -p 5672 -c "nc 10.1.0.254 5672"

