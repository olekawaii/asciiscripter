# asciiscripter tutorial

ASCR is a pure lazy statically-typed functional language to generate colorful
ascii animations from text files. Check out some examples in the gifs/ directory.

There is an example output at `gifs/new.aa` `./main.sh` and a linux binary at `./ascr`

Let's start with a demonstration. Create a file named `bird.ascr` and inside 
paste:

```
bird: gif
9 5 2
    ,,   ....11...
  <')    ..381....
   ( \   ...101...
---YY----666336666
     |\  .....11..
end
```

Then run `ascr -n bird bird.ascr`. You should see the image for a few seconds and
it should save to a file `bird.sh`. Opening the file you should see:

```sh
#!/bin/sh
printf '    \033[31m,,\n  \033[33m<\033[37m\047\033[31m)\n   ( \134\n\033[36m---\033[33mYY\033[36m----\n     \033[31m|\134\n'
```

The command genrated a posix shell script. Let's break down the language:

## Comments
Comments start with `---` and end with `<o>`. The eye closes and reopens again.

## Code Block
Syntax of a code block:

```
<name>: <type>
<content>
end
```

The order of code blocks doesn't matter.

`content` is either art or code. Here is an example of an art block:

```
colors: gif
6 1 4
red   111...green 22222.
yellow333333blue  4444..
end
```
```
name:              colors
type:              gif
width:             6
height:            1
number of frames:  4
the first frame:   "red   " colored red
```
The three numbers representing width, height, and frames, are followed by <frames>
rectangles of size (width * 2, height). The left half is the art and the right
are the corresponding ansi colours, the colors being:
```
. transparent
0 black
1 red
2 green
3 yellow
4 blue
5 magenta
6 cyan
7 white
? rainbow
```
The rectangles can be stacked left right, up down, or both (first left right 
then up down). 

Code blocks on the other hand are just a string of words

## Type System
```
type = gif | int | bool | color | fn type type
```
The type of a number is `int` but the the type of a function that takes a gif 
and returns an int is `fn gif int`, using prefix notation. All functions are 
curried; to write a function that takes an (int and a color) and returns a gif
you'd write `fn int fn color gif`. Aka a function that takes an int and returns
a (function that takes a color and returns a gif). Functions can return functions
and accept functions as arguments; a function that takes a (function from an int 
to a color) and returns a gif would be `fn fn int color gif`. Note the lack of
paranthesies :)

## Function Syntax

`$x` is used to access the xth argument. The first index is 1

Example:
```
slow_and_move_up: fn gif gif
slow 2 move 0 1 $1  
end
```
Type signiturens:
```
slow: fn int fn gif gif
2: int
move: fn int fn int fn gif gif
$1: gif
```
What this really is is 
```
slow 2 (move 0 1 $1)
```
or noncurried python style
```py
slow(2, move(0, 1, $1))
```
The function `slow` takes two arguments, `2 : int` and `move 0 1 $1 : gif`. The 
function `move` takes three: `0 : int`, `1 : int`, and `$1 : gif`. $1 being the 
first argument of the `slow_and_move_up` function. Like types, there are no 
paranthesies. Paranthesies are inferred from known types.

all these evaluate to 3:
```
add 1 2

add 1 add 1 1

add add 1 1 1        aka        add (add 1 1) 1
```
## Builtins
```
move: fn int fn int fn gif gif
  Takes three inputs, an x, y, and a gif. It offsets the entire gif by the x and y

slow: fn int fn gif gif
  Takes an int and gif. Replicates every frame in a gif by the int. `slow 2 x` 
  would double every frame and will produce a gif of x2 framecount

join: fn gif fn gif gif
  Takes two gifs and layers them on top of each other, the first argument being
  on top. Transparent squares will be replaced by the lower layer. It will also 
  make the gif loopable; joing two gifs of lengths 3 and 4 will produce a gif 
  of length 12 frames. That way after 12 frames both will be at the start again.

seq: fn gif fn gif gif
  Takes two gifs and sequences them one after the other

null: gif
  A gif with one transparent frame. Useful for sequencing

take: fn int fn gif gif
  Takes the first $1 frames of the looping gif

dye: fn color fn gif gif
  takes a color and dyes the entire gif to that color, excluding transparency

skip: fn int fn gif gif
  puts the first frame at the end $1 times

reverse: fn gif gif
  reverses the gif

frame_count: fn gif int
  returns the gif's number of frames. Useful for debugging
```

This concludes the intro. Here's a juggling_robot animation. Use 
`ascr -n <name> <path to this file>` to browse and tinker with it.

````
robot: gif
5 7 8 
 c O .6.5.  O  ..5.. O o .5.6.o   o5...6
  V  ..5..  |  ..5..  V  ..5.. \./ .526.
[_ ]]77072 [ ] .702.[[_ ]72202[__ ]22202
 ( ) .5.6.  \  ..6..  \  ..6.. \ / .6.5.
  \  ..5.. ( ) .5.6. ( ) .5.6.  \  ..6..
 ( ) .6.5.  \  ..5..  \  ..5.. ( ) .5.6.
  \  ..6.. ( ) .6.5. ( ) .6.5.  \  ..5..

 c O .5.6.  O  ..6.. O o .6.5.o   o6...5
  V  ..6..  |  ..6..  V  ..6.. \./ .675.
[_ ]]22027 [ ] .207.[[_ ]27707[__ ]77707
 ( ) .6.5.  \  ..5..  \  ..5.. \ / .5.6.
  \  ..6.. ( ) .6.5. ( ) .6.5.  \  ..5..
 ( ) .5.6.  \  ..6..  \  ..6.. ( ) .6.5.
  \  ..5.. ( ) .5.6. ( ) .5.6.  \  ..6..
end

dress_white: gif
5 4 2
 ._. .777. ._. .777.
 ) ( .777. ) ( .777.
//..\77777/,.\\77777
/,|\\77777//|.\77777
end

dress_red: gif
5 4 1
 ._. .111.
 | | .111.
//W\\11111
.....
end

---
adding an empty row to `dress_red` so it's a drop in replacement for `dress_white`.
gifs are spawened with the bottom left corner at (1, 1) 
<o>

right_arm: gif
5 2 8 
 |   .7...  /  ..7..    /....7    /....7
W    2....VV   22...\/\/ 2222.\/\/ 2222.

/    ....7  /  ..70. /   .700. |   .7...
\/\/ 2222.VV   22...W    2....W    2....
end

left_arm: gif
5 3 8 
     .....     .....__   77...__   77...
 __  .77.. __  .77..  \__..222  \__..222
   \/...22   \/...22     .....     .....

 |   07...  /  007..     .....     .....
  \__.0222  \__..222  |  ..7..  \  ..7..
     .....     .....   \/...22   \/...22
end

legs: gif
3 1 4
 )\.22
( )2.2
/( 22.
( )2.2
end

ball: gif
1 1 3
O7c7.7
end

ball_cycle: gif
seq dye red ball 
seq take 5 null
seq dye blue ball
seq take 5 null
seq dye yellow ball 
  take 5 null
end

---
I could have also written `ball_cycle` as 

ball_cycle: gif
seq dye red ball seq take 5 null seq dye blue ball seq take 5 null seq dye yellow ball take 5 null
end

It's the same thing
<o>

wheel: gif
join helper 0  2  5  join helper 1  3  5 
join helper 2  4  5  join helper 3  5  6 
join helper 5  7  6  join helper 6  8  6 
join helper 7  9  6  join helper 8  10 5 
join helper 9  11 4  join helper 10 13 5 
join helper 11 13 6  join helper 12 13 7 
join helper 13 13 8  join helper 14 12 9 
join helper 15 11 10 join helper 16 9  11
join helper 17 7  11 join helper 18 5  11
join helper 19 3  10 join helper 20 2  9 
join helper 21 1  8  join helper 22 1  7
helper 23 1  6 
end

helper: fn int fn int fn int gif
skip $1 move $2 $3 ball_cycle
end

---
Lots of repetition. I made it into a function.
<o>

robot_juggler: fn gif gif
join move 12 1 signiture
join skip 3 move 1 3 left_arm
join skip 5 move 9 3 right_arm
join slow 2 move 5 4 robot
join wheel
join move 5 1 slow 4 $1
     slow 6 move 6 1 legs
end

---
made `robot_juggler` into a function so I can choose the dress that gets displayed

here's how it would look as tree diagram

join 
  move 
    12 
    1 
    signiture
  join 
    skip 
      3 
      move 
        1 
        3 
        left_arm
    join 
      skip 
        5 
        move 
          9 
          3 
          right_arm
      join 
        slow 
          2 
          move 
            5 
            4 
            robot
        join 
          wheel
          join 
            move 
              5 
              1 
              slow 
                4 
                $1
            slow 
              6 
              move 
                6 
                1 
                legs

<o>

main: gif
robot_juggler dye red dress_white
end

---
Try changing the dress to `dress_red` or changing the color
<o>

signiture: gif
2 1 1
OB77
end
````
