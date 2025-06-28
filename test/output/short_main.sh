#!/bin/sh

if [ $(tput cols) -lt 12 -o $(tput lines) -lt 1 ]
then
    printf "\33[91mterminal is too small\nmust be at least 12 by 1 cells\33[0m\n" >&2
    exit 1
fi

stty -echo
printf '\33[?25l'

move_up="\33[0F"

cleanup() {
    printf "$move_up\33[0J\33[0m\33[?25h"
    stty echo
    exit 0
}

trap cleanup INT

printf '\33[91mh\33[92me\33[93ml\33[94ml\33[95mo \33[96mw\33[97mo\33[91mr\33[92ml\33[93md\33[94m!'
sleep 2
cleanup