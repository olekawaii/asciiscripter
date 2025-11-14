#!/bin/sh

if [ $(tput cols) -lt 16 -o $(tput lines) -lt 7 ]; then
    printf "\33[91mterminal is too small\nmust be at least 16 by 7 cells\33[0m\n" >&2
    exit 1
fi

stty -echo
printf '\33[?25l'

move_up="\33[6F"

cleanup() {
    printf "$move_up\33[0J\33[0m\33[?25h"
    stty echo
    exit 0
}

trap cleanup INT

printf '  \33[91m__\33[97mc\33[91m__\33[97m,\33[91m___\n(_\33[97mc\33[91m_\33[97m,O\33[91m_/\134_\33[97m3\33[91m_)\n     \33[97m| |\n    /_. \134\n     | |    \33[91m__\n     \33[97m|"|  \33[91m(\33[97mc\33[91m_\33[97mo\33[91m_)\n     \33[97m| |   (..)'
sleep 2
cleanup