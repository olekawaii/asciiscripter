{-# Language MultiWayIf, LambdaCase #-}

import System.Exit (die)
import System.Process (callCommand)
import System.Environment (getArgs)
import Control.Applicative (liftA2)
import Control.Monad ((<=<), (>=>), guard, unless, when)
import Control.Arrow ((<<<), (>>>), (***), (&&&), (+++), (|||))
import qualified Data.Map.Strict as Map 
import Data.Bifunctor (first, second, bimap)
import Data.Either (fromRight, fromLeft)
import Data.Maybe (isJust, fromJust, listToMaybe, isNothing, fromMaybe)
import Data.Tuple (swap)
import Data.List (find, nub, transpose, sortOn, intercalate)
import Text.Read (readMaybe) 

instance Show Character where
  show Space = " "
  show (Character char color) = colour color (show char)

instance Show a => Show (Colored a) where
  show (Colored color c) = colour color (show c)

colour :: Color -> String -> String 
colour c s = colorCode c <> s <> "\x1b[0m"

colorCode :: Color -> String
colorCode Black   = "\x1b[30m"
colorCode Red     = "\x1b[31m"
colorCode Green   = "\x1b[32m"
colorCode Yellow  = "\x1b[33m"
colorCode Blue    = "\x1b[34m"
colorCode Magenta = "\x1b[35m"
colorCode Cyan    = "\x1b[36m"
colorCode White   = "\x1b[37m"

format :: String -> [String] -> String
format [] _ = []
format ('%':xs) [] = error xs
format ('%':xs) (a:as) = a <> format xs as
format (x:xs) as = x : format xs as

get :: (Eq a) => a -> HashMap a b -> Maybe b
get a (HashMap list) = fmap snd $ find ((== a) . fst) list

insert :: (Eq a) => a -> b -> HashMap a b -> Maybe (HashMap a b)
insert a b hashmap@(HashMap list) = 
  if isJust (get a hashmap) 
  then Nothing
  else pure $ HashMap ((a, b) : list)

keys :: HashMap a b -> [a]
keys (HashMap list) = map fst list

new :: HashMap a b
new = HashMap []

infix 8 ...

main :: IO ()
main = getContents >>= \x -> let gif = (parseVideo . words) x in
  case changeFormat gif of
    (width, height, newGif) ->
      let
        file = "output" <> ".sh"
        shortFile = "short-" <> file
        (fileSh, command) = formatShell defaultMods width height Nothing . pipeline $ newGif
      in
      (
        if width > 80
        then putStrLn "\x1b[33;1mWarning:\x1b[0m video should not be over 80 chars in width"
        else if height > 24
          then putStrLn "\x1b[33;1mWarning:\x1b[0m video should not be over 24 chars in height"
          else pure ()
      ) >>
      writeFile file fileSh >>
      writeFile shortFile command >>
      callCommand ("chmod +x " <> file <> " " <> shortFile) >>
      putStrLn "\x1b[92mcompiled"

parseVideo :: [String] -> RealGif
parseVideo ("empty_video": xs) = [] 
parseVideo ("cons_frame": xs) = let (frame, leftover) = parseFrame xs in frame : parseVideo leftover

parseFrame :: [String] -> (Map Coordinate Character, [String])
parseFrame ("unsafe_cons_cell" : xs) = 
  let (cell, leftover) = parseCell xs in 
  first (cell :) (parseFrame leftover)
parseFrame ("empty_frame" : xs) = ([], xs)

parseCell :: [String] -> ((Coordinate, Character), [String])
parseCell ("cell" : xs) =
  let (coord, leftover1) = parseCoordinate xs in
  let (character, leftover2) = parseChar leftover1 in 
  ((coord, character), leftover2)

parseCoordinate :: [String] -> (Coordinate, [String])
parseCoordinate ("coordinate": xs) = 
  let (x, leftover1) = parse_int xs in
  let (y, leftover2) = parse_int leftover1 in
  ((x, y), leftover2)

parse_int :: [String] -> (Int, [String])
parse_int ("positive" : xs) = parseNat xs
parse_int ("negative" : xs) = first (* (-1)) (parseNat xs)
parse_int ("zero" : xs) = (0, xs)

parseNat :: [String] -> (Int, [String])
parseNat ("succ": xs) = first (1 +) (parseNat xs)
parseNat ("one": xs) = (1, xs)

parseChar :: [String] -> (Character, [String])
parseChar ("space": xs) = (Space, xs)
parseChar ("char": char : color : xs) = 
  let 
    c = case char of 
       "bang" -> '!'
       "double_quotes" -> '"'
       "pound" -> '#'
       "dollar" -> '$'
       "percent" -> '%'
       "ampersand" -> '&'
       "single_quote" -> '\''
       "open_paranthesis" -> '('
       "close_paranthesis" -> ')'
       "asterisk" -> '*'
       "plus" -> '+'
       "comma" -> ','
       "hyphen" -> '-'
       "period" -> '.'
       "slash" -> '/'
       "digit_zero" -> '0'
       "digit_one" -> '1'
       "digit_two" -> '2'
       "digit_three" -> '3'
       "digit_four" -> '4'
       "digit_five" -> '5'
       "digit_six" -> '6'
       "digit_seven" -> '7'
       "digit_eight" -> '8'
       "digit_nine" -> '9'
       "colon" -> ':'
       "semicolon" -> ';'
       "less_than" -> '<'
       "equals" -> '='
       "greater_than" -> '>'
       "question_mark" -> '?'
       "at_sign" -> '@'
       "uppercase_a" -> 'A'
       "uppercase_b" -> 'B'
       "uppercase_c" -> 'C'
       "uppercase_d" -> 'D'
       "uppercase_e" -> 'E'
       "uppercase_f" -> 'F'
       "uppercase_g" -> 'G'
       "uppercase_h" -> 'H'
       "uppercase_i" -> 'I'
       "uppercase_j" -> 'J'
       "uppercase_k" -> 'K'
       "uppercase_l" -> 'L'
       "uppercase_m" -> 'M'
       "uppercase_n" -> 'N'
       "uppercase_o" -> 'O'
       "uppercase_p" -> 'P'
       "uppercase_q" -> 'Q'
       "uppercase_r" -> 'R'
       "uppercase_s" -> 'S'
       "uppercase_t" -> 'T'
       "uppercase_u" -> 'U'
       "uppercase_v" -> 'V'
       "uppercase_w" -> 'W'
       "uppercase_x" -> 'X'
       "uppercase_y" -> 'Y'
       "uppercase_z" -> 'Z'
       "opening_bracket" -> '['
       "backslash" -> '\\'
       "closing_bracket" -> ']'
       "caret" -> '^'
       "underscore" -> '_'
       "grave_accent" -> '`'
       "lowercase_a" -> 'a'
       "lowercase_b" -> 'b'
       "lowercase_c" -> 'c'
       "lowercase_d" -> 'd'
       "lowercase_e" -> 'e'
       "lowercase_f" -> 'f'
       "lowercase_g" -> 'g'
       "lowercase_h" -> 'h'
       "lowercase_i" -> 'i'
       "lowercase_j" -> 'j'
       "lowercase_k" -> 'k'
       "lowercase_l" -> 'l'
       "lowercase_m" -> 'm'
       "lowercase_n" -> 'n'
       "lowercase_o" -> 'o'
       "lowercase_p" -> 'p'
       "lowercase_q" -> 'q'
       "lowercase_r" -> 'r'
       "lowercase_s" -> 's'
       "lowercase_t" -> 't'
       "lowercase_u" -> 'u'
       "lowercase_v" -> 'v'
       "lowercase_w" -> 'w'
       "lowercase_x" -> 'x'
       "lowercase_y" -> 'y'
       "lowercase_z" -> 'z'
       "opening_brace" -> '{'
       "vertical_bar" -> '|'
       "closing_brace" -> '}'
       "tilde" -> '~'
   in 
   let 
     col = case color of
        "black" -> Black
        "red" -> Red
        "green" -> Green
        "yellow" -> Yellow
        "blue" -> Blue
        "magenta" -> Magenta
        "cyan" -> Cyan
        "white" -> White
   in
      (Character c col, xs)

dimensions :: RealGif -> (Int, Int, Int, Int)
dimensions gif = case concatMap (map fst) gif of
  [] -> (0, 0, 0, 0)
  g  -> getDimensions g
    where
      getDimensions :: [Coordinate] -> (Int, Int, Int, Int)
      getDimensions x =
        let
          xCoords = map fst x
          yCoords = map snd x
        in
          (
            minimum xCoords,
            maximum xCoords,
            minimum yCoords,
            maximum yCoords
          )

getMark :: Marked x -> Mark
getMark (Marked m _) = m

defaultMods :: Modifiers
defaultMods = Modifiers {
  frameTime = 0.2,
  directory = ".",
  message   = False,
  quiet     = False,
  check     = False,
  text      = False,
  target    = "main",
  output    = Looping
}

-- check that it's not 0
changeFormat :: RealGif -> (Int, Int, [[[Character]]])
changeFormat x = let (x_min, x_max, y_min, y_max) = dimensions x in
  let
    chart = liftA2 (flip (,))  [y_max, y_max -1 .. y_min] [x_min..x_max]
    convertFrame :: Map Coordinate Character -> [[Character]]
    convertFrame a =  chunksOf (x_max - x_min + 1) $ map lookupChar chart
      where
        lookupChar want = fromMaybe Space $ lookup want a
  in
  (x_max - x_min + 1, y_max - y_min + 1, map convertFrame x) 

number :: FilePath -> String -> [Marked String]
number f = zipWith (\x y -> Marked File {origin = f, line = x, block = Nothing} y) [1..] . lines

cons = (:)

unwrap :: Marked a -> a
unwrap (Marked _ a) = a  
    
legalNameChars = '_' : ['a'..'z'] <> ['0'..'9'] 

(...) = (.).(.)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n x  = uncurry ((. chunksOf n) . cons) $ splitAt n x

formatShell :: Modifiers -> Int -> Int -> Maybe String -> [Either String Int] -> (ShellScript, ShellScript)
formatShell mods wd ht message renderedFrames = case renderedFrames of
  [Left frame] -> (init2 <> gif <> "\\n'", init2 <> hideprompt <> initMove <> cleanup <> gif <> "'\nsleep 2" <> "\ncleanup")
    where gif = "printf '" <> frame
  frames  -> (init2 <> hideprompt <> initMove <> cleanup <> intro <> alloc <> loop <> body <> done, init2 <> hideprompt <> initMove <> cleanup <> intro <> alloc <> body <> "cleanup")
    where 
      newHelper :: [Either String Int] -> String
      newHelper [] = ""
      newHelper (Left s : xs) = "    draw '" <> s <> "'\n" <> newHelper xs
      newHelper (Right i : xs) = "    sleep " <> show (fromIntegral i * frameTime mods) <> "\n" <> newHelper xs
      intro = "draw() {\n    printf \"$move_up$1\"\n    sleep " <> 
        show (frameTime mods) <> "\n}\n\n"
      alloc = "yes '' | head -n " <> show (ht - 1) <> "\n\n"
      loop = "while true; do\n"
      body = newHelper frames
      done = "done"
  where
    hideprompt = "stty -echo\nprintf '\\33[?25l'\n\n"
    initMove = "move_up=\"\\33[" <> show (ht - 1) <> "F\"\n\n"
    cleanup = "cleanup() {\n    printf \"$move_up\\33[0J\\33[0m\\33[?25h\"\n    stty echo\n    exit 0\n}\n\ntrap cleanup INT\n\n"
    comment = "#!/bin/sh\n" <> maybe "" (('\n' :) . unlines . map ("# " <>) . lines) message <> "\n"
    sizeCheck = "if [ $(tput cols) -lt " <> show wd <> " -o $(tput lines) -lt " <> show ht <> " ]; then\n    printf \"\\33[91mterminal is too small\\nmust be at least " <> show wd <> " by " <> show ht <> " cells\\33[0m\\n\" >&2\n    exit 1\nfi\n\n" 
    init2 = comment <> sizeCheck

pipeline :: [[[Character]]] -> [Either String Int]
pipeline input = case reduce2 input of
  [x] -> pure . Left . fromMaybe "" . formatCommands . intercalate [Move Down] . map (map Draw . removeExtraSpaces) $ x
  (x:xs) -> (Left (tail . tail $ renderer x) :) . init . countUp . map formatCommands . reduce $ (x:xs)
  [] -> []

countUp :: [Maybe String] -> [Either String Int]
countUp [] = []
countUp (Just s: xs) = Left s : countUp xs
countUp (Nothing: xs) = let (num, other) = first ((+ 1) . length) $ span isNothing xs in Right num : countUp other
      
data Command = Draw Character | Move Dir
data Dir = Down | Next

reduce :: [[[Character]]] -> [[Command]]
reduce [] = []
reduce [x] = pure . intercalate [Move Down] . map (map Draw) $ map removeExtraSpaces x
reduce (x:xs) = reverse . map (uncurry (helper Black)) $ chunksOf2 (x: reverse (x: xs))
  where 
    helper :: Color -> [[Character]] -> [[Character]] -> [Command]
    helper _ [] [] = []
    helper _ ([[]]) ([[]]) = []
    helper c ([]:xs) ([]:ys) = Move Down : helper c xs ys
    helper c ((x:xx):xs) ((y:yy):ys) = 
        if x == y 
        then 
          let 
            (same, different) = first (map fst) . span (uncurry (==)) $ zip (x:xx) (y:yy) 
            len = length same
            leftover = uncurry (helper c) . bimap (: xs) (: ys) $ unzip different
          in 
          if different /= [] && sum (map (getSize c) same) < 6 
          then map Draw same <> leftover
          else take len (repeat $ Move Next) <> leftover
        else Draw x : helper (case getColor x of Nothing -> c; Just x -> x) (xx: xs) (yy: ys)

getSize :: Color -> Character -> Int
getSize c = \case
    Space -> 1
    Character char color -> (if color == c then 0 else 6) + case char of
        '%'  -> 2
        '\'' -> 3
        '\\' -> 4
        _ -> 1

getColor = \case
  Space -> Nothing
  Character _ color -> Just color

formatCommands :: [Command] -> Maybe String
formatCommands x = if all isMove x then Nothing else pure $ showCommands x Black
  where
    isMove (Move _) = True
    isMove _ = False

showCommands :: [Command] -> Color -> String
showCommands [] _ = []
showCommands m@(Move x : xs) oldColor = case cleanMove m of 
  (f, []) -> show f {next = 0}
  (f, other) -> show f <> showCommands other oldColor
  -- where (str, other) = cleanMove m
showCommands (Draw x : xs) oldColor = case x of
  Space -> ' ' : showCommands xs oldColor
  c@(Character char color) -> 
    if color == oldColor 
    then clean char <> showCommands xs oldColor 
    else colorChar c <> showCommands xs color 

data FinalMovement = Final {next :: Int, down :: Int} 

instance Show FinalMovement where
  show Final {next = next, down = down} = y <> x
    where 
      y = case down of 
        0 -> "" 
        1 -> "\\n"
        2 -> "\\n\\n"
        _ -> "\\33[" <> show down <> "E" 
      x = if next == 0 then "" else "\\33[" <> show next <> "C"

cleanMove :: [Command] -> (FinalMovement, [Command])
cleanMove x = first (flip makeFinal Final {next = 0, down = 0}) $ helper x
  where 
    helper :: [Command] -> ([Dir], [Command])
    helper (Move x: xs) = first (x:) $ helper xs
    helper x = ([], x)

    makeFinal :: [Dir] -> FinalMovement -> FinalMovement
    makeFinal [] f = f
    makeFinal (Next : xs) f = makeFinal xs f {next = next f + 1, down = down f} 
    makeFinal (Down : xs) f = makeFinal xs f {next = 0, down = down f + 1} 
    
renderer :: [[Character]] -> String
renderer = helper Black . concatMap (Character '\n' Black :)
  where
    helper :: Color -> [Character] -> String
    helper _        [] = "" --"\\n"
    helper oldcolor (Space:xs) = ' ' : helper oldcolor xs
    helper oldcolor (c@(Character char color) : xs) =
      if color == oldcolor || char == '\n'
      then clean char <> helper oldcolor xs
      else  colorChar c <> helper color xs

chunksOf2 :: [a] -> [(a, a)] 
chunksOf2 [] = []
chunksOf2 [a] = []
chunksOf2 (a:b:bs) = (a, b) : chunksOf2 (b:bs)

reduce1 :: [[[Character]]] -> [[[Character]]]
reduce1 = map (map removeExtraSpaces)

reduce2 :: [[[Character]]] -> [[[Character]]]
reduce2 [] = []
reduce2 (x:xs) = if all (== x) xs then [x] else x:xs

removeExtraSpaces :: [Character] -> [Character]
removeExtraSpaces = reverse . remove . reverse
  where
    remove (Space : as) = remove as
    remove as = as

colorChar :: Character -> String
colorChar Space = " "
colorChar (Character s c) = "\\33[9" <> case c of
    Black   -> "0"
    Red     -> "1"
    Green   -> "2"
    Yellow  -> "3"
    Blue    -> "4"
    Magenta -> "5"
    Cyan    -> "6"
    White   -> "7"
  <> "m" <> clean s

clean :: Char -> String
clean '\\' = "\\134"
clean '\'' = "\\47"
clean '\n' = "\\n"
clean '%'  = "%%"
clean a    = pure a

type ShellScript   = String
type Map a b       = [(a,b)]

data HashMap a b = HashMap [(a, b)]

data Modifiers = Modifiers {
  target      :: String,
  frameTime   :: Float,
  directory   :: FilePath,
  message     :: Bool,
  quiet       :: Bool,
  check       :: Bool,
  text        :: Bool,
  output      :: OutputType
} deriving Show

data OutputType = Single | Looping deriving Show

type Coordinate    = (Int,Int)
type RealGif = [Map Coordinate Character]


data SimpleType = Int | Giff | Colour | Direction deriving Eq

data Direction = East | West | North | South deriving (Show, Eq)

data ReturnType = I Int | G RealGif | C Color | D Direction -- deriving Show
data Character = Space | Character Char Color deriving Eq

type LineNumber    = Int

data Mark 
  = Arguments
  | None
  | File {
      origin :: FilePath, 
      line   :: LineNumber, 
      block  :: Maybe Name
    }

data Marked a = Marked Mark a -- deriving Show

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White 
  deriving (Show, Eq)

data Colored a = Colored Color a deriving Eq
type Name = String
