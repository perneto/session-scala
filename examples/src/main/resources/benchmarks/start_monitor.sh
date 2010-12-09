#!/bin/bash

. include.sh

#(cd SessML; svn up && make)


socat TCP4-LISTEN:5672,fork,reuseaddr TCP4:10.1.0.254:5672
echo "$(hostname): socat exited"
