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
import Data.List (find, nub, transpose, sortOn, intercalate, singleton)
import Text.Read (readMaybe)

getVideo :: IO (Int, Int, [[[Character]]])
getVideo = getContents >>= pure . parse . words

parse :: [String] -> (Int, Int, [[[Character]]])
parse ("the" : "list" : "frame" : xs) = changeFormat . map convertNewFrame . fst . parseNewVideo $ xs
parse ("the" : "frame" : xs) = changeFormat . pure . convertNewFrame . fst . parseNewFrame $ xs
parse ("the" : "final_video" : xs) = parseNewNewFinalVideo $ xs

parseNewNewFinalVideo :: [String] -> (Int, Int, [[[Character]]])
parseNewNewFinalVideo ("unsafe_final_video" : "graphical_options" : font : theme : xs) =
    let out = parseNewNewVideo xs in out

parseNewNewVideo :: [String] -> (Int, Int, [[[Character]]])
parseNewNewVideo s = let (out, _) = parseListGrid s in
    changeFormat $ map numberLines out
    where
      numberChars :: [Character] -> [(Int, Character)]
      numberChars x = zip [1 ..] x

      numberLines :: [[Character]] -> [((Int, Int), Character)]
      numberLines xs =
          concat
          $ map (\(n, xs) -> map ((\(n2, c) -> ((n2, n), c))) (numberChars xs))
          $ zip [1 ..] xs

parseListGrid :: [String] -> ([[[Character]]], [String])
parseListGrid ("nil" : xs) = ([], xs)
parseListGrid ("cons": xs) =
    let (out, leftover) = parseGrid xs in
        first (out :) (parseListGrid leftover)
parseListGrid (x: _) = error x

parseGrid :: [String] -> ([[Character]], [String])
parseGrid ("nil" : xs) = ([], xs)
parseGrid ("cons": xs) =
    let (out, leftover) = parseNewRow xs in
        first (out :) (parseGrid leftover)

parseNewRow :: [String] -> ([Character], [String])
parseNewRow ("nil" : xs) = ([], xs)
parseNewRow ("cons": xs) =
    let (out, leftover) = parseChar xs in
        first (out :) (parseNewRow leftover)

dimensions :: [Map Coordinate Character] -> (Int, Int, Int, Int)
dimensions gif = case concatMap (map fst) $ map (filter (\(_, x) -> isSpace x)) gif of
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

isSpace Space = False
isSpace _     = True

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Show, Eq)

type Frame = (Column, Column)

type Column = [Horizontal]

type Horizontal = ([Character], [Character])

type Video = [Frame]



parseGridCell :: [String] -> (Character, [String])
parseGridCell ("empty_grid_cell": xs) = (Space, xs)
parseGridCell ("filter_grid_cell": "<lambda>" : xs) = (Space, xs)
parseGridCell ("full_grid_cell": xs) = parseChar xs

parseRow :: [String] -> ([Character], [String])
parseRow ("nil" : xs) = ([], xs)
parseRow ("cons" : xs) = let (char, other) = parseGridCell xs in
    first (char :) (parseRow other)

parseHorizontal :: [String] -> (Horizontal, [String])
parseHorizontal ("horizontal" : xs) =
    let
        (a, other)  = parseRow xs
        (b, other2) = parseRow other
    in
        ((a, b), other2)

parseColumn :: [String] -> (Column, [String])
parseColumn ("nil" : xs) = ([], xs)
parseColumn ("cons" : xs) = let (hor, other) = parseHorizontal xs in
    first (hor :) (parseColumn other)

parseNewFrame :: [String] -> (Frame, [String])
parseNewFrame ("frame" : xs) =
    let
        (a, other)  = parseColumn xs
        (b, other2) = parseColumn other
    in
        ((a, b), other2)

parseNewVideo :: [String] -> (Video, [String])
parseNewVideo ("nil" : xs) = ([], xs)
parseNewVideo ("cons" : xs) = let (frame, other) = parseNewFrame xs in
    first (frame :) (parseNewVideo other)
parseNewVideo _ = error "\x1b[91mshould be of type video starting with either single or prepend\x1b[0m"


convertNewFrame :: Frame -> Map Coordinate Character
convertNewFrame (a, b) =
    filter (\(a, b) -> case b of
        Space -> False
        _ -> True) $
    (concatMap
      (\(y, (left, right)) -> map (\(a, b) -> ((a, y), b)) (zip [0, -1 ..] left <> zip [1..] right)) $
      zip [-1, -2 ..] a) ++
    (concatMap
      (\(y, (left, right)) -> map (\(a, b) -> ((a, y), b)) (zip [0, -1 ..] left <> zip [1..] right)) $
      zip [0..] b)



parseChar :: [String] -> (Character, [String])
parseChar ("cell_space": xs) = (Space, xs)
parseChar ("cell": char : color : xs) =
  let
    c = case char of
       "space_char" -> ' '
       "exclamation_mark" -> '!'
       "quotation_mark" -> '"'
       "number_sign" -> '#'
       "dollar_sign" -> '$'
       "percent_sign" -> '%'
       "ampersand" -> '&'
       "apostrophe" -> '\''
       "left_paranthesis" -> '('
       "right_paranthesis" -> ')'
       "asterisk" -> '*'
       "plus_sign" -> '+'
       "comma" -> ','
       "hyphen_minus" -> '-'
       "full_stop" -> '.'
       "solidus" -> '/'
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
       "less_than_sign" -> '<'
       "equals_sign" -> '='
       "greater_than_sign" -> '>'
       "question_mark" -> '?'
       "commercial_at" -> '@'
       "capital_a" -> 'A'
       "capital_b" -> 'B'
       "capital_c" -> 'C'
       "capital_d" -> 'D'
       "capital_e" -> 'E'
       "capital_f" -> 'F'
       "capital_g" -> 'G'
       "capital_h" -> 'H'
       "capital_i" -> 'I'
       "capital_j" -> 'J'
       "capital_k" -> 'K'
       "capital_l" -> 'L'
       "capital_m" -> 'M'
       "capital_n" -> 'N'
       "capital_o" -> 'O'
       "capital_p" -> 'P'
       "capital_q" -> 'Q'
       "capital_r" -> 'R'
       "capital_s" -> 'S'
       "capital_t" -> 'T'
       "capital_u" -> 'U'
       "capital_v" -> 'V'
       "capital_w" -> 'W'
       "capital_x" -> 'X'
       "capital_y" -> 'Y'
       "capital_z" -> 'Z'
       "left_square_bracket" -> '['
       "reverse_solidus" -> '\\'
       "right_square_bracket" -> ']'
       "circumflex_accent" -> '^'
       "low_line" -> '_'
       "grave_accent" -> '`'
       "small_a" -> 'a'
       "small_b" -> 'b'
       "small_c" -> 'c'
       "small_d" -> 'd'
       "small_e" -> 'e'
       "small_f" -> 'f'
       "small_g" -> 'g'
       "small_h" -> 'h'
       "small_i" -> 'i'
       "small_j" -> 'j'
       "small_k" -> 'k'
       "small_l" -> 'l'
       "small_m" -> 'm'
       "small_n" -> 'n'
       "small_o" -> 'o'
       "small_p" -> 'p'
       "small_q" -> 'q'
       "small_r" -> 'r'
       "small_s" -> 's'
       "small_t" -> 't'
       "small_u" -> 'u'
       "small_v" -> 'v'
       "small_w" -> 'w'
       "small_x" -> 'x'
       "small_y" -> 'y'
       "small_z" -> 'z'
       "left_curly_brace" -> '{'
       "vertical_line" -> '|'
       "right_curly_brace" -> '}'
       "tilde" -> '~'
       x -> error x
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

parseChar x = error (show (take 3  x))
