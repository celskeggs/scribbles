#!/bin/bash

if [ "$1" = "no" ]
then
	for x in *.rkt
	do
		sed -i "s/^#lang typed\/racket$/#lang typed\/racket\/no-check/" $x
	done
elif [ "$1" = "yes" ]
then
	for x in *.rkt
	do
		sed -i "s/^#lang typed\/racket\/no-check$/#lang typed\/racket/" $x
	done
else
	echo -e "Usage:\t$0 no\n\t$0 yes"
	exit 1
fi
