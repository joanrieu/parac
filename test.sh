#!/bin/bash
set -e
OUT=a.out

./ParaC.sh "$@" |& tee "$OUT.s"
grep -q Exception "$OUT.s" && exit
gcc -m32 -o "$OUT" "$OUT.s" src/ParaC.c -lpthread -g
./$OUT
