{-# Language MultiWayIf #-}

module Main (main) where

-- import Lib

import Control.Monad ((<=<))
import Control.Arrow ((<<<),  (***), (&&&), (+++), (|||))
import Data.Bifunctor (first, second)
import Text.Read (readMaybe) 
import Data.Maybe
import Data.Tuple
import System.IO
import Data.List

import Types

infix 8 ...

-- main = interact $ show ||| show . map (name *** getDependencies . map snd) <<< parse <=< cutSpace 

printBody =  
  show ||| 
    concat 
    . map (renderer . uncurry colorize) 
    . uncurry (\h -> map ((\x -> (h,x)) . snd)) 
    . fromJust
    . find (eq "shooting_underscore" . name . fst) 
  <<< parse <=< cutSpace

main = readFile "gifs/bird.aa" >>= putStr . printBody

cons = (:)
append :: a -> [a] -> [a]
append  x y = y <> [x]
eq :: Eq a => a -> a -> Bool
eq = (==)

parse :: Lines -> Either Error (Map Header Lines)   
parse []           = return []
parse ((l,x) : xs) = parseHeader l x >>= \header -> case head . words . snd <$> listToMaybe xs of
  Nothing      -> Left $ Custom "Header is lacking a body" l
  Just "frame" -> getDelimiter xs l "script" "end" [] >>= \(script, other) -> 
                    cons (header, script) <$> parse other
  Just _       -> cons (header, take len xs) <$> parse (drop len xs)
    where len = height header * frames header

-- finds closing delimiter and returs up to and after it
-- getDelimiter :: Lines -> LineNumber -> original -> good -> [bad] -> Either Error (garbage, Lines)
getDelimiter :: Lines -> LineNumber -> String -> String -> [String] -> Either Error (Lines, Lines)
getDelimiter []           olden original _     _  = Left $ Delimiter original olden
getDelimiter ((l,x) : xs) olden original good bad = case words x of
  [w] -> if 
    | w == good  -> return ([],xs)
    | elem w bad -> Left $ Delimiter original olden
  _   -> first (append (l,x)) <$> getDelimiter xs olden original good bad   

cutSpace :: String -> Either Error Lines
cutSpace = cleanGood . zip [1..] . lines
  where
    cleanGood []           = Right []
    cleanGood ((l,x) : xs) = case words x of 
      []      -> cleanGood xs
      ["com"] -> getDelimiter xs l "com" "moc" ["com"] >>= cleanGood . snd
      ["moc"] -> Left $ Delimiter "moc" l
      _       -> cons (l,x) <$> cleanGood xs
    
getDependencies :: [String] -> [Name]
getDependencies 
  = filter (\x -> 
      x `notElem` ["frame"] && 
      head x `notElem` "'\"")
  . words
  . filter (flip notElem (['0'..'9'] <> ['A'..'Z'] <> ",.!-+" <> ['\n','"']))
  . concat

-- if something has no dependencies it can be calculated
win :: EpicGifData -> Dependencies -> NamedLines -> Either Error EpicGifData
win uwu []          _ = pure uwu
win uwu ((n,[]):xs) d = solve uwu n (d ! n) >>= \s -> win (s:uwu) (second (filter (/= n)) <$> xs) d
win uwu (x:xs)      d = win uwu (append x xs) d

(...) = (.).(.)

(!) = fromJust ... flip lookup

solve :: EpicGifData -> Name -> Lines -> Either Error (Name, Gif)
solve = undefined

renderer :: [Colored Char] -> String
renderer x = "\x1b[0m" <> helper x White
  where
    helper :: [Colored Char] -> Color -> String
    helper [] _ = "\x1b[0m\n"
    helper (Colored color char :xs) oldcolor = 
      if color == oldcolor 
      then char : helper xs oldcolor
      else colorChar (Colored color char) <> helper xs color
-- every used gif and its dependencies

colorize :: Header -> String -> [Colored Char]
colorize h = map (uncurry Colored) <<< uncurry zip <<< first (map charToColor) <<< swap <<< splitAt (width h)-- <<< lines
  where 
    charToColor x = case x of 
      '0' -> Black 
      '1' -> Red
      '2' -> Green
      '3' -> Yellow
      '4' -> Blue
      '5' -> Magenta
      '6' -> Cyan
      '7' -> White
      '.' -> Transp
      _   -> White

colorChar :: Colored Char -> String
colorChar c = case c of
  Colored Black   s -> append s "\x1b[30m"
  Colored Red     s -> append s "\x1b[31m"
  Colored Green   s -> append s "\x1b[32m"
  Colored Yellow  s -> append s "\x1b[33m"
  Colored Blue    s -> append s "\x1b[34m"
  Colored Magenta s -> append s "\x1b[35m"
  Colored Cyan    s -> append s "\x1b[36m"
  Colored White   s -> append s "\x1b[37m"
  Colored Transp  s -> append s "\x1b[30m"


parseHeader :: LineNumber -> String -> Either Error Header
parseHeader l = first ($ l) . parseHelper . words
  where 
    parseHelper :: [String] -> Either (LineNumber -> Error) Header
    parseHelper [a,b,c,d] = case readMaybe a of 
      Nothing    -> Left $ Parse "width"  "an int" (show a) 
      Just width -> case readMaybe b of 
        Nothing     -> Left $ Parse "height" "an int" (show b)
        Just height -> case readMaybe c of 
          Nothing     -> Left $ Parse "number of frames" "an int" (show c) 
          Just frames -> if
            | any (`notElem` '_' : ['a'..'z']) d -> Left $ Parse "name" "only chars a-z and _" (show d)
            | otherwise -> Right Header {
                width  = width, 
                height = height,
                frames = frames,
                name   = d
              }
    parseHelper w = Left $ Parse "header" "4 values" (show $ length w) 
{-
 - tree structure
 - could be texture.uwu or script.uwuscript
 - both have the same output, a size and frameorder
 - calling script file is recursive
 - a script file can compile to a texture file whick compiles to a gif
 - a script can't use itself
-}
