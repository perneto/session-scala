#!/bin/bash

source include.sh

session-scala_update
sessml_update

nc -l -p 5672 -c "nc 10.1.0.254 5672"

