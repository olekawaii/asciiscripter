#!/bin/sh
draw() { 
  printf "\033[3A\r\033[0J$1"
  sleep 0.2
}
printf '\n\n\n'
while true
do
  draw '\033[31mo   o\n \033[34m\134_/\n[   ]\n'
  draw ' \033[31mo O\n  \033[34mV\n[  ]]\n'
  draw '  \033[31mO\n  \033[34m|\n [ ]\n'
  draw ' \033[31mO o\n  \033[34mV\n[[  ]\n'
done
