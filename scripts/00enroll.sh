#!/bin/sh

echo "generating private key"
conex generate private $1
echo "generating public key"
conex generate key $1
echo "signing public key"
conex sign key -k $1 $1
