#!/bin/sh

if [ $(tput cols) -lt 23 -o $(tput lines) -lt 7 ]; then
    printf "\33[91mterminal is too small\nmust be at least 23 by 7 cells\33[0m\n" >&2
    exit 1
fi

printf '  \33[91m__\33[97mc\33[91m__\33[97m,\33[91m___\n(_\33[97mc\33[91m_\33[97m,O\33[91m_/\134_\33[97m3\33[91m_)\n     \33[97m| |       \33[90mmuchomor\n    \33[97m/_. \134     \33[90molekawaii\n     \33[97m| |    \33[91m__\n     \33[97m|"|  \33[91m(\33[97mc\33[91m_\33[97mo\33[91m_)\n     \33[97m| |   (..)\n'