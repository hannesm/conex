#!/bin/sh

for j in $(cd /tmp/ta ; ls) ; do
  conex sign $1 $2 -k $j -t /tmp/ta
done
