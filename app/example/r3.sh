#!/bin/sh
printf '\n\n'
while true
do
  printf '\033[3A\r\033[0J\033[31mo   o\n \033[34m\134_/\n[   ]\n'
  sleep 0.2
  printf '\033[3A\r\033[0J \033[31mo O\n  \033[34mV\n[  ]]\n'
  sleep 0.2
  printf '\033[3A\r\033[0J  \033[31mO\n  \033[34m|\n [ ]\n'
  sleep 0.2
  printf '\033[3A\r\033[0J \033[31mO o\n  \033[34mV\n[[  ]\n'
  sleep 0.2
done
