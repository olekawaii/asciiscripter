#!/bin/sh

if [ $(tput cols) -lt 21 -o $(tput lines) -lt 7 ]
then
    printf "\033[91mterminal is too small\nmust be at least 21 by 7 cells\033[0m\n" >&2
    exit 1
fi

printf '   \033[91m__\033[97mc\033[91m__\033[97m,\033[91m___\n (_\033[97mc\033[91m_\033[97m,O\033[91m_/\134_\033[97m3\033[91m_)\n      \033[97m| |\n     /_. \134   Muchomor\n      | |    \033[91m__   \033[97movb\n      |"|  \033[91m(\033[97mc\033[91m_\033[97mo\033[91m_)\n      \033[97m| |   (..)\n'