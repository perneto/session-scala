#!/bin/bash

. include.sh

#(cd SessML; svn up && make)


nc.traditional -l -k -p 5672 -c "nc 10.1.0.254 5672"
echo "$(hostname): netcat exited"
