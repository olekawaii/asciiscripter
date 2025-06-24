cd src
ghc -O main.hs
cp main /usr/local/bin/ascr
cd ..
cp uwu /usr/local/bin/uwu
printf 'moved executables `ascr` and `uwu` to ~/.local/bin/\n'
