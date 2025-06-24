# asciiscripter tutorial

Work in progress. Check out some examples in the art/ directory

## Install
Dependencies: git, ghc
```sh
git clone https://github.com/olekawaii/asciiscripter.git
cd asciiscripter
scripts/build.sh
sudo scripts/install.sh
```
The compiler is `ascr`. Use `ascr -h` to see the options. `uwu` a simple build
tool (see `uwu help`).

## First project
To create your project run
```sh
uwu new first_project && cd first_project
````
Inside you will find 
```
first_project/       root
|-- .ascr            options for `ascr` when using `uwu run`
|-- output/          directory containing the output files
|   `-- main.sh      output of main.ascr
`-- src/             directory containing the source code
    `-- main.ascr    main file
```
Compile and run the main function with `uwu run` (from anywhere in the project). 
You should see a success message with a short animation. The output shell script 
hould appear in `output/main.sh`.

## Syntax
The language is very simple; there are no conditionals, pattermatching,
recursion, or generics.

### Types
There are only a handful of types and you can't create your own (for now). Those 
types are:
```
int        integer. data int = one | succ int
frames     a sequence of frames, a frame being a Map Coordinate Character
fn a b     a function from a b
bool       true | false
color      black | green | yellow | blue | magenta | cyan | white
```
Types use Polish notation. a function from an `int` to a `bool` is `fn int
bool`. A function from (a function from an `int` to a `bool`) to `frames` is
`fn fn int bool frames`, which in standard notation would be `fn (fn int bool)
frames`. Functions are curried; `add : fn int fn int int` is the addition
function. `add 3 4 = 7` however `add 3 : fn int int` is a partially-applied 
function which when given a 4 evaluates to a 7.

### Values
syntax for declaring a variable
```
five : int
  5
end

add_five : fn int int
  add five $1
end
```
`$1` refers to the first argument of the function.   
Here is an art block
```
bird : frames
9 5
    ,,   ....11...
  <')    ..371....
   ( \   ...1/1...
---YY----222332222
     |\  .....11..
end

three_frames : frames
9 5
1   ,,   7...11...2   ,,   7...11...
  <')    ..371....  <')    ..371....
   ( \   ...1/1...   ( \   ...1/1...
---YY----222332222---YY----222332222
     |\  .....11..     |\  .....11..
3   ,,   7...11...
  <')    ..371....
   ( \   ...1/1...
---YY----222332222
     |\  .....11..
end
```
the `9 5` under the name are the length ang height of a frame. Every pixel in
the art has a corresponding color, the colors being
```
0 black
1 red
2 green
3 yellow
4 blue
5 magenta
6 cyan
7 white
. transparent
/ space
```

### Comments
```
-- single-line comment

---
this is a
multi-line
comment
<o>

bottom_of_triangle : frames
6 2
--   /\  ..11..   make sure it's `-- ` and not `--` to make its length odd
 /  \ .1//1.
/____\111111
```

## Built-in values
```
join : fn frames fn frames frames
```
`join a b` layers a on top of b and makes the output video loopable. Joining a 
(video with 2 frames) and (one with 3 frames) results in a (video with 6 frames) so
it loops after after every 6 frames.
```
seq : fn frames fn frames frames
```
`seq a b` combines a and b so that b plays after a
```
dye : fn color fn frames frames
```
`dye a b` recolors every pixel in b to the color a
```
null : frames
```
`null` is a single transparent frame. Useful for `seq`
```
anchor : frames
```
`anchor` is a single frame with a space character. Useful for expanding the
field of view 
```
move : fn int fn int fn frames frames
```
`move a b c` moves the video c (a in the x, b in the y)
```
skip : fn int fn frames frames 
```
`skip a b` moves the first a frames of b to the back
```
take : fn int fn frames frames
```
`take a b` returns the first a frames of the looping video b
```
reverse : fn frames frames
```
`reverse` reverses the frames
```
loop : fn frames frames
```
`loop a` sequences a with the trimmed (reverse of a) making a smooth 
animathion

