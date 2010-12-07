#!/bin/bash

push() {
  hg push ssh://$1/session-scala
  ssh $1 "cd session-scala && hg up"
}
push shade02
push shadexx
push shade04
push treebeard
push matrix13
push matrix16
