#!/bin/sh

halt_node() {
  ssh $1 "sudo halt" <<EOF
omp08
EOF
}

for node in shade02 shade04 shadexx shade07 treebeard matrix13 matrix16; do
  halt_node $node
done
