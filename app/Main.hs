{-# Language MultiWayIf #-}

module Main (main) where

-- import Lib

import Control.Monad
import Control.Arrow
import Text.Read
import Data.Maybe
import Data.Bifunctor
-- import System.IO
import Data.Tuple

import Types


main = interact $ fromEither . bimap show (show . map (name *** getDependencies . map snd)) . (cutSpace >=> parse)

fromEither :: Either a a -> a
fromEither (Left  a) = a
fromEither (Right a) = a

cons = (:)
append :: a -> [a] -> [a]
append  x y = y <> [x]

parse :: RawData -> Either Error (Map Header RawData)   
parse []           = return []
parse ((l,x) : xs) = parseHeader l x >>= \header -> case head . words . snd <$> listToMaybe xs of
  Nothing      -> Left $ Custom "Header is lacking a body" l
  Just "frame" -> getDelimiter xs l "script" "end" [] >>= \(script, other) -> cons (header, script) <$> parse other
  Just _       -> cons (header, take len xs) <$> parse (drop len xs)
    where len = height header * frames header

-- finds closing delimiter and returs up to and after it
-- getDelimiter :: RawData -> LineNumber -> original -> good -> [bad] -> Either Error (garbage, RawData)
getDelimiter :: RawData -> LineNumber -> String -> String -> [String] -> Either Error (RawData, RawData)
getDelimiter []           olden original _     _  = Left $ Delimiter original olden
getDelimiter ((l,x) : xs) olden original good bad = case words x of
  [w] -> if 
    | w == good  -> return ([],xs)
    | elem w bad -> Left $ Delimiter original olden
  _   -> bimap (append (l,x)) id <$> getDelimiter xs olden original good bad   

cutSpace :: String -> Either Error RawData
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
      x `notElem` ["DRAW", "PLAY", "SHIFT", "frame", "CLEAR"] && 
      head x `notElem` "'\"")
  . words
  . filter (flip notElem "1234567890,.!-+")
  . unlines

-- if something has no dependencies it can be calculated
win :: EpicGifData -> Dependencies -> NamedRawData -> Either Error EpicGifData
win uwu []          _ = pure uwu
win uwu ((n,[]):xs) d = solve uwu n (d ! n) >>= \s -> win (s : uwu) (bimap id (filter (/= n)) <$> xs) d
win uwu (x:xs)      d = win uwu (append x xs) d

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
parseHeader l = bimap ($ l) id . parseHelper . words
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
