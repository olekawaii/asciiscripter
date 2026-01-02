{-# Language MultiWayIf, LambdaCase #-}

module Parse where

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

getVideo :: IO (Int, Int, [[[Character]]])
getVideo = getContents >>= pure . changeFormat . parseVideo . words 

dimensions :: [Map Coordinate Character] -> (Int, Int, Int, Int)
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

-- check that it's not 0
changeFormat :: [Map Coordinate Character] -> (Int, Int, [[[Character]]])
changeFormat x = let (x_min, x_max, y_min, y_max) = dimensions x in
  let
    chart = liftA2 (flip (,))  [y_max, y_max -1 .. y_min] [x_min..x_max]
    convertFrame :: Map Coordinate Character -> [[Character]]
    convertFrame a =  chunksOf (x_max - x_min + 1) $ map lookupChar chart
      where
        lookupChar want = fromMaybe Space $ lookup want a
  in
  (x_max - x_min + 1, y_max - y_min + 1, map convertFrame x) 

cons = (:)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n x  = uncurry ((. chunksOf n) . cons) $ splitAt n x

type Map a b       = [(a,b)]

data OutputType = Single | Looping deriving Show

type Coordinate    = (Int,Int)

data Character = Space | Character Char Color deriving Eq

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White 
  deriving (Show, Eq)

parseVideo :: [String] -> [Map Coordinate Character]
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

