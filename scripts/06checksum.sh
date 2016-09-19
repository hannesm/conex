#!/bin/sh

conex generate checksum $1
conex sign checksum $1 -k $2
