{-# Language MultiWayIf #-}

module Main (main) where

import Lib

import Control.Monad
import Control.Arrow
import Text.Read
import Data.Maybe
import Data.Bifunctor
import System.IO
import Data.Tuple

import Types


main 
  = interact 
  $ fromEither 
  . bimap show (show . map (\(x,(_,list)) -> (x, getDependencies $ map snd list))) 
  . (cutSpace >=> parse)

fromEither :: Either a a -> a
fromEither (Left  a) = a
fromEither (Right a) = a
-- main = readFile "image.aa" >>= putStr . show . head . parse . cutSpace "image.aa" 

cons = (:)
app :: [a] -> a -> [a]
app  x y = x <> [y]

parse :: RawData -> Either Error (Map Name Body)   
parse []           = return []
parse ((l,x) : xs) = parseHeader l x >>= \a -> case (head . words . snd) <$> listToMaybe xs of
  Nothing      -> Left $ Custom "Header does not have a body" l
  Just "frame" -> scriptList >>= \script -> (cons (name a, (a, script)) <$> parse (drop (length script + 1) xs))
    where 
      scriptList = fmap fst $ getDelimiter xs l "script" "end" []
  -- Just "frame" -> Left $ Custom "I found the beginning of a script" l
  Just _       -> cons (name a, (a, take (height a * frames a) xs)) <$> parse (drop (height a * frames a) xs)

-- finds closing delimiter and returs upt to and after it
-- getDelimiter :: RawData -> LineNumber -> original -> good -> [bad] -> Either Error (garbage, RawData)
getDelimiter :: RawData -> LineNumber -> String -> String -> [String] -> Either Error (RawData, RawData)
getDelimiter []           olden original _     _  = Left $ Delimiter original olden
getDelimiter ((l,x) : xs) olden original good bad = case words x of
  w | length w > 1 || w == [] -> bimap (<> [(l,x)]) id <$> getDelimiter xs olden original good bad
    | elem (head w) bad       -> Left $ Delimiter original olden
    | head w == good          -> return ([],xs)
    | otherwise               -> bimap (<> [(l,x)]) id <$> getDelimiter xs olden original good bad   


cutSpace :: String -> Either Error RawData
cutSpace = cleanGood . zip [1..] . lines
  where
    cleanGood []           = Right []
    cleanGood ((l,x) : xs) = case words x of 
      []      -> cleanGood xs
      ["com"] -> getDelimiter xs l "com" "moc" ["com"] >>= cleanGood . snd
      ["moc"] -> Left $ Delimiter "moc" l
      _       -> ((l,x) :) <$> cleanGood xs
    
getDependencies :: [String] -> [Name]
getDependencies 
  = filter (\x -> 
      x `notElem` ["DRAW", "PLAY", "SHIFT", "frame", "CLEAR"] && 
      head x `notElem` "'\"")
  . words
  . filter (flip notElem "1234567890,.!-+")
  . unlines

-- if something has no dependencies it can be calculated
win :: EpicGifData -> Dependencies -> NamedRawData -> Either Error EpicGifData
win uwu []          _ = pure uwu
win uwu ((n,[]):xs) d = solve uwu n (d ! n) >>= \s -> win (s : uwu) (bimap id (filter (/= n)) <$> xs) d
win uwu (x:xs)      d = win uwu (xs <> [x]) d

(!) :: Eq a => Map a b -> a -> b
(!) m a = snd . head $ filter ((== a) . fst) m

solve :: EpicGifData -> Name -> RawData -> Either Error (Name, Gif)
solve = undefined

-- every used gif and its dependencies

colorize :: Header -> String -> [[[Colored Char]]]
colorize h s = undefined
  . print
  . fmap (\x -> map swap $ zip (take (width h) x) (drop (width h) x))
  . fmap (take $ width h)
  $ lines s

parseHeader :: LineNumber -> String -> Either Error Header
parseHeader line s = case words s of 
  c | length c /= 4 -> Left $ Parse "header" "4 values" (show $ length c) line
    | isNothing width -> Left $ Parse "width"  "an int" (show $ c !! 0) line
    | isNothing height -> Left $ Parse "height" "an int" (show $ c !! 1) line
    | isNothing frames -> Left $ Parse "number of frames" "an int" (show $ c !! 2) line 
    | any (`notElem` '_' : ['a'..'z']) name -> Left $ Parse "name" "only chars a-z and _" (show name) line
    | otherwise -> Right Header {
        width  = fromJust width, 
        height = fromJust height,
        frames = fromJust frames,
        name   = name
      }
    where
      width  = readMaybe (c !! 0)
      height = readMaybe (c !! 1)
      frames = readMaybe (c !! 2)
      name   = c !! 3
    
{-
 - tree structure
 - could be texture.uwu or script.uwuscript
 - both have the same output, a size and frameorder
 - calling script file is recursive
 - a script file can compile to a texture file whick compiles to a gif
 - a script can't use itself
-}
