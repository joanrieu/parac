#!/bin/bash
set -e
OUT=a.out

exec 3>&1
exec >"$OUT.s"
./ParaC.sh "$@"
exec 1>&3
cat "$OUT.s"
gcc -m32 -o "$OUT" "$OUT.s" src/ParaC.c -lpthread -g
./$OUT
