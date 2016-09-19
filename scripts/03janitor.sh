#!/bin/sh

JS=""

rm -rf /tmp/ta
mkdir /tmp/ta

for var in "$@" ; do
  conex generate private $var
  conex generate key $var
  conex sign key -k $var $var
  JS=`echo -n "$JS --id $var"`
  cp /tmp/conex/keys/$var /tmp/ta
done

conex generate team janitors $JS

for var in "$@" ; do
  for k in "$@" ; do
    #cross sign all the keys!
    conex sign key -k $var $k
  done
  conex sign team -k $var janitors
  #mv ~/.conex/tmp.conex.$var.private ~/.conex/bak.tmp.conex.$var.private
done

conex show team janitors
conex verify -t /tmp/ta
