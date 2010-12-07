#!/bin/bash

. include.sh

#(cd SessML; svn up && make)


mkfifo backpipe
nc -l 5672 0<backpipe | nc 10.1.0.254 5672 1>backpipe

