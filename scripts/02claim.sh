#!/bin/sh

conex generate authorisation $1 --id $2
conex sign authorisation $1 -k $2
