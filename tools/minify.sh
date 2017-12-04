#!/bin/sh
if [ "$#" -ne 1 ];
then
	echo "Usage: $0 source" >&2
	exit -1
fi
~/sources/picotool/p8tool --debug luamin ~/sources/pico8/carts/$1.p8 > /tmp/$1_symbols

cat /tmp/$1_symbols | awk -F "\'" 'NR>1 {print "\""$2"\";\""$4"\""}' > /tmp/$1_tokens

