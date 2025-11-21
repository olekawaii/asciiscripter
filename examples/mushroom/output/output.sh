#!/bin/sh

if [ $(tput cols) -lt 23 -o $(tput lines) -lt 8 ]; then
    printf "\33[91mterminal is too small\nmust be at least 23 by 8 cells\33[0m\n" >&2
    exit 1
fi

printf '  \33[91m__\33[97mc\33[91m__\33[97m,\33[91m___\n(_\33[97mc\33[91m__\33[97mO\33[91m_/\134_\33[97m3\33[91m_)\n       \33[97m.\n     | |   thorn v0.1.0\n    / __\134\n       .   .\33[91m__\n     \33[97m| |  \33[91m(__\33[97mo\33[91m_)\n     \33[97m| |   (  )\n'