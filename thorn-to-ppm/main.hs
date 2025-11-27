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

showNum :: Int -> String
showNum x = take (5 - length s) (cycle ['0']) <> s
  where s = show x

makeFiles :: String -> [String] -> Int -> IO ()
makeFiles _ [] _ = pure ()
makeFiles header (x:xs) n = writeFile fileName content >> makeFiles header xs (n + 1)
    where 
        content = header <> x
        fileName = "output" <> showNum n <> ".ppm"

main :: IO ()
main = getContents >>= \x -> let gif = (parseVideo . words) x in
  case changeFormat gif of
    (width, height, newGif) ->
        let gifFrames = map pipelineGif newGif in
        let header = "P3\n" <> show (width * 9) <> " " <> show (height * 14) <> "\n255\n" in
        makeFiles header gifFrames 0

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
    
(...) = (.).(.)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n x  = uncurry ((. chunksOf n) . cons) $ splitAt n x

pipelineGif :: [[Character]] -> String
pipelineGif = unlines . map showColor . uwu

uwu :: [[Character]] -> [Color]
uwu x = concat . map (concat . foldr1 combine . map toBitmap) $ x

combine :: [[Color]] -> [[Color]] -> [[Color]] 
combine x y = map (uncurry (++)) $ zip x y

showColor :: Color -> String
showColor Black    = " 40  40  40"
showColor Red      = "234 105  98"
showColor Green    = "169 182 101"
showColor Yellow   = "216 166  87"
showColor Blue     = "125 174 163"
showColor Magenta  = "211 134 155"
showColor Cyan     = "137 180 130"
showColor White    = "212 190 152"

isSpace Space = True
isSpace (Character _ _) = False

countUp :: [Maybe String] -> [Either String Int]
countUp [] = []
countUp (Just s: xs) = Left s : countUp xs
countUp (Nothing: xs) = let (num, other) = first ((+ 1) . length) $ span isNothing xs in Right num : countUp other
      
data Command = Draw Character | Move Dir
data Dir = Down | Next

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

toBitmap :: Character -> [[Color]]
toBitmap Space = [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black],
                  [Black, Black, Black, Black, Black, Black, Black, Black, Black]]
toBitmap (Character char c) = case char of
  '!' ->  [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '"' ->  [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , Black, Black, c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '#' ->  [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '$' ->  [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, Black, c    , Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, c    , Black, Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black]]

  '%'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '&'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '\'' -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '('  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  ')'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '*'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [c    , c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '+'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  ','  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black]]

  '-'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '.'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '/'   -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, c    , Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [c    , c    , Black, Black, Black, Black, Black, Black, Black],
           [c    , Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '0'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '1'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '2'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '3'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '4'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '5'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '6'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '7'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '8'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '9'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  ':'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  ';'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '<'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '='  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '>'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '?'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '@'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'A'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'B'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'C'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, Black, c    , Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, Black, c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'D'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, c    , c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'E'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, Black, c    , Black],
           [Black, Black, c    , c    , Black, c    , Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'F'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, Black, c    , Black],
           [Black, Black, c    , c    , Black, c    , Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'G'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, Black, c    , Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, Black, c    , c    , c    , Black, c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'H'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'I'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'J'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'K'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'L'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'M'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , Black, c    , c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'N'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, Black, c    , c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'O'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'P'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'Q'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , c    , c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'R'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'S'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'T'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , Black, c    , c    , Black, c    , Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'U'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'V'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'W'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'X'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'Y'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'Z'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '['  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '\\' -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  ']'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '^'  -> [[Black, Black, Black, Black, c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '_'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [c    , c    , c    , c    , c    , c    , c    , c    , Black]]

  '`'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'a'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'b'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'c'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'd'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'e'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'f'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'g'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black]]

  'h'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'i'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'j'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black]]

  'k'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'l'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'm'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , Black, c    , c    , Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'n'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'o'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'p'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , Black, Black, Black, Black]]

  'q'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , c    , c    , Black]]

  'r'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, c    , c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  's'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  't'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, c    , c    , Black],
           [Black, Black, Black, Black, c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'u'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'v'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, Black, c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'w'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'x'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , c    , Black, Black, Black],
           [Black, Black, c    , c    , Black, c    , c    , Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  'y'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, c    , c    , Black, Black, Black, c    , c    , Black],
           [Black, Black, c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, c    , c    , Black],
           [Black, Black, Black, Black, Black, c    , c    , Black, Black],
           [Black, Black, c    , c    , c    , c    , Black, Black, Black]]

  'z'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, c    , c    , Black, Black, c    , c    , Black, Black],
           [Black, Black, Black, Black, c    , c    , Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, c    , c    , Black, Black, c    , c    , Black],
           [Black, c    , c    , c    , c    , c    , c    , c    , Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '{'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, c    , c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '|'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '}'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, c    , c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, Black, c    , c    , c    , Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, Black, Black, c    , c    , Black, Black, Black, Black],
           [Black, c    , c    , c    , Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]

  '~'  -> [[Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, c    , c    , c    , Black, c    , c    , Black],
           [Black, c    , c    , Black, c    , c    , c    , Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black],
           [Black, Black, Black, Black, Black, Black, Black, Black, Black]]
