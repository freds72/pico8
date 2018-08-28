#!/bin/sh
if [ "$#" -ne 1 ];
then
	echo "Usage: $0 source" >&2
	exit -1
fi

cat $1 | sed -E -n -f minify_rules.sed


