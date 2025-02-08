# asciiscripter

Convert ascii drawings to shell script images and videos

try converting some files in the gifs directory

## Examples

create a file called 'testfile' and inside paste

```
5 3 4 robothead
gif
o   o1...1
 \_/ .444.
[   ]40004

 o O .1.1.
  V  ..4..
[  ]]40044

  O  ..1..
  |  ..4..
 [ ] .404.

 O o .1.1. 
  V  ..4..
[[  ]44004
fig
```

then run 

```
ascr robothead testfile
```

this will generate the file 'robothead.sh'

```
#!/bin/sh


draw() {
  printf "\033[3A\r$1"
  sleep 0.2
}
printf '\n\n\n\033[0m'
while true #\\
do #\\
  draw '\033[31mo   o\n \033[34m\134_/\n[   ]\n'
  sleep 0.2
  draw ' \033[31mo O \n  \033[34mV \n[  ]]\n'
  sleep 0.2
  draw '  \033[31mO  \n  \033[34m| \n [ ] \n'
  sleep 0.2
  draw ' \033[31mO o \n  \033[34mV \n[[  ]\n'
  sleep 0.2
done #\\
```

running this file will play the gif
