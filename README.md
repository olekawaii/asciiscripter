# asciiscripter

Convert ascii drawings to shell script images and videos

try converting some files in the gifs directory

## Examples

To create a comment use the deliminators com and moc. Asciiscripter converts ascii art files into shell scripts that can be run anywhere. Let's create a simple gif of a colourful spinning robot head. first create a file named 'demo'. Paste this inside:

```
5 3 4 robot_head
gif
 o O .1.1.  O  ..1.. O o .1.1.o   o1...1
  V  ..7..  |  ..7..  V  ..7.. \_/ .777.
[_ ]]77077 [ ] .707.[[_ ]77707[__ ]77707
fig
```

Now run 'ascr robot_head example'. You should get a success message and the 
gif should play. Looking at ./robot_head.sh you should see:


```
#!/bin/sh

draw() {
  printf "\033[3A\r$1"
  sleep 0.2
}
printf '\n\n\n\033[0m'
while true #\\
do #\\
  draw '  \033[36mO  \n  \033[37m| \n [ ] \n'
  draw ' \033[36mO \033[35mo \n  \033[37mV \n[[_ ]\n'
  draw '\033[36mo   \033[35mo\n \033[37m\134_/\n[__ ]\n'
  draw ' \033[35mo \033[36mO \n  \033[37mV \n[\033[32m_ \033[37m]]\n'
done #\\
```


The gif was converted to shell script. Let's break down the syntax. every code
block is a gif. the first line is the header consisting of the gif's 
width, height, frame count, and name. Then the block surrounded by either 'gif'
or 'scr'. The 'gif' block is made up of rectangles with the same height as the 
header's height and a width of 2x the header's width. The left side is the art 
and the right are the colours. These are the colours:

. -> Transparent
0 -> Black
1 -> Red
2 -> Green
3 -> Yellow
4 -> Blue
5 -> Magenta
6 -> Cyan
7 -> White

You can arrange there rectangles vertically, horizontally, or both (read left
-> right and up -> down). 

Let's add a neck gif. either before or after robot_head add a new block (how 
code blocks are arranged doesn't matter):

```
3 4 8 robot_neck
gif
( )6.5 \ .5. \ .5.\ /5.6
 \ .6.( )6.5( )6.5 \ .5.
( )5.6 \ .6. \ .6.( )6.5
 \ .5.( )5.6( )5.6 \ .6.

( )5.6 \ .6. \ .6.\ /6.5
 \ .5.( )5.6( )5.6 \ .6.
( )6.5 \ .5. \ .5.( )5.6
 \ .6.( )6.5( )6.5 \ .5.
fig
```

you can run 'ascr robot_neck example' to the result.

Now let's merge them together using a script. Here's how 
scripts work:

* There are numbered layers that can each hold one gif.
* Every new frame, all the gifs advance by one frame.
* Highter layers overshadow lower ones.
* Run commands on layers, the syntax being
  layer COMMAND [args]

```
5 7 8 robot_upper
scr
  frame 1
    1 DRAW 2 1 robot_neck
    2 DRAW 1 5 robot_head
  frame 2 8
rcs
```

this gif composed the two gif into one. You can later create a body script that
will compose some other gif with the robot_upper gif to create a robot_body gif.
