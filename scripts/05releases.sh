#!/bin/sh

REL="--id tls.0.1.0 --id tls.0.7.1"

conex generate releases $1 $REL
conex sign releases $1 -k $2
