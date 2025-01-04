{-# Language MultiWayIf #-}

module Main (main) where

-- import Lib
import Control.Monad ((<=<), (>=>))
import Control.Arrow ((<<<),  (***), (&&&), (+++), (|||))
import Data.Bifunctor (first, second)
import Text.Read (readMaybe) 
import Data.Maybe
import Data.Tuple
import System.IO
import Data.List
import System.Environment

import Types

infix 8 ...

unwrapNotated (Drawings x) = x
unwrapNotated (Script x) = x

number :: FilePath -> String -> [Marked String]
number f = zipWith (\x y -> Marked Mark {origin = f, line = x} y) [1..] . lines

main = getArgs >>= \arguments -> case arguments of 
  []                         -> putStr . show $ MissingArgs 0
  [_]                        -> putStr . show $ MissingArgs 1
  (".-i":target:args)        -> 
    zipWith number args <$> mapM readFile args >>= (
      putStr 
      <<< 
      show ||| show
      <<< flip dependenciesOf target . map (name *** fmap (map unwrap))
      <=< parse
      <=< cutSpace
      <<< concat
    )
  (target:args)              -> 
    zipWith number args <$> mapM readFile args >>= (
    putStr <<< show ||| id <<< uwu target <<< concat)

uwu :: Name -> [Marked String] -> OrError String
uwu target = cutSpace >=> parse >=> lookupName . map (second unwrapNotated) >=>
  pure . 
  (\x -> "echo -e '" <> x <> "'\n") . 
  renderer . 
  map (uncurry parseColorLine) .   
  uncurry (\h -> map ((\x -> (width h,x)) . unwrap))
    where
      lookupName x = case find (eq target . name . fst) x of
        Nothing -> Left $ NoMatchingName target
        Just x  -> pure x

(>?) :: Mark -> OrToError a -> OrError a
(>?) x y = first ($ x) y

cons = (:)
append :: a -> [a] -> [a]
append  x y = y <> [x]
eq :: Eq a => a -> a -> Bool
eq = (==)

parse :: [Marked String] -> OrError (Map Header (Notated [Marked String])) 
parse []                = pure []
parse (Marked m l : xs) = m >? parseHeader l >>= 
  \header -> case words . unwrap <$> listToMaybe xs of
    Nothing         -> Left $ Custom "Header is lacking a body" m
    Just ["scr"] -> m >? getDelimiter (tail xs) "rcs" >>= \(script, other) -> 
                         cons (header,(Script script)) <$> parse other
    Just _          -> cons (header,(Drawings $ take len xs)) <$> parse (drop len xs)
      where len = height header * frames header

-- if something has no dependencies it can be calculated
win :: EpicGifData -> Dependencies -> Map Header (Notated [Marked String]) -> OrError EpicGifData
win uwu []          _ = pure uwu
win uwu ((n,[]):xs) d = solve uwu `uncurry` (fromJust . find ((== n) . name . fst)) d >>= \s -> 
                        win (s:uwu) (second (filter (/= n)) <$> xs) d
win uwu (x:xs)      d = win uwu (append x xs) d

-- finds closing delimiter and returs up to and after it
-- getDelimiter :: Lines -> good -> OrError (inside, outside)
getDelimiter :: [Marked String] -> String -> OrToError ([Marked String], [Marked String])
getDelimiter []     good  = Left $ Delimiter (reverse good)
getDelimiter (x:xs) good  = case words $ unwrap x of
  [w] -> if 
    | w == good  -> pure ([],xs)
    | w == reverse good -> Left $ Delimiter (reverse good)
    | otherwise  -> first (cons x) <$> getDelimiter xs good   
  _   -> first (cons x) <$> getDelimiter xs good

cutSpace :: [Marked String] -> OrError [Marked String]
cutSpace []                    = Right []
cutSpace (x@(Marked m l) : xs) = case words $ unwrap x of 
  []      -> cutSpace xs
  ["com"] -> m >? getDelimiter xs "moc" >>= cutSpace . snd
  ["moc"] -> Left $ BadDelimiter "moc" m
  _       -> cons x <$> cutSpace xs

unwrap :: Marked a -> a
unwrap (Marked _ a) = a  
    
dependenciesOf :: Map Name (Notated [String]) -> Name -> OrError (Map Name [Name])
dependenciesOf table = fmap nub . getDependencies []
  where
    getDependencies :: [Name] -> Name -> OrError (Map Name [Name])
    getDependencies used target = if elem target used then Left $ Recursive target else
      case extractDependencies <$> lookup target table of 
        Nothing -> Left $ NoMatchingName target
        Just x  -> fmap (cons (target,x) . concat) . sequence . map (getDependencies (target : used)) $ x

concatEither :: [Either a [b]] -> Either a [b]
concatEither = foldl fn (Right []) 
  where 
    fn :: Either a [b] -> Either a [b] -> Either a [b]
    fn x y = case x of 
      Left a -> Left a
      Right a -> Right $ a <> fromRight y

    fromRight (Right x) = x

legalNameChars = '_' : ['a'..'z'] <> ['0'..'9'] 

isValidName :: Name -> Bool
isValidName x = all ($ x) 
  [ flip notElem ["frame","com","moc","scr","rcs"]
  , all (`elem` legalNameChars)
  , any (`elem` ['a'..'z'])
  ]

extractDependencies :: Notated [String] -> [Name]
extractDependencies (Drawings _) = []
extractDependencies (Script x)   = filter isValidName . map reverse . flip helper [] . unlines $ x
  where 
    helper :: String -> Name -> [Name]
    helper []     _ = []
    helper (x:xs) w = if x `elem` legalNameChars
                      then helper xs (x:w)
                      else w : helper xs []

(...) = (.).(.)

-- (!) = fromJust ... flip lookup

solve :: EpicGifData -> Header -> Notated [Marked String] -> OrError (Name, Gif)
solve = undefined

renderer :: [[Colored Char]] -> String
renderer = helper Transp . concatMap (append (Colored Transp '\n') . removeExtraSpaces)
  where
    helper :: Color -> [Colored Char] -> String
    helper _        []                       = "" --"\\n"
    helper oldcolor (Colored color char :xs) = 
      if color == oldcolor || elem char " \n" || color == Transp
      then clean char <> helper oldcolor xs
      else colorChar (Colored color char) <> helper color xs
    
    removeExtraSpaces :: [Colored Char] -> [Colored Char]
    removeExtraSpaces = reverse . remove . reverse
      where
        remove (Colored _ ' ' : as) = remove as
        remove as = as


clean c = case c of 
  '\\' -> "\\\\"
  '\'' -> "'\\''"
  '\n' -> "\\n"
  a    -> [a]

-- every used gif and its dependencies

parseColorLine :: Int -> String -> [Colored Char]
parseColorLine = uncurry (zipWith Colored) . first (map charToColor) . swap ... splitAt
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

test :: Int -> Bool
test i = case putStr "" of 
  _ -> True
parseHeader :: String -> OrToError Header
parseHeader = parseHelper . words
  where
    parseHelper :: [String] -> OrToError Header
    parseHelper [a,b,c,d] = case readMaybe a of 
      Nothing     -> Left $ Parse "width" "an int" (colour Magenta (show a)) 
      Just width  -> case readMaybe b of 
        Nothing     -> Left $ Parse "height" "an int" (colour Magenta (show b)) 
        Just height -> case readMaybe c of 
          Nothing     -> Left $ Parse "number of frames" "an int" (colour Magenta (show c)) 
          Just frames -> if
            | not $ isValidName d -> 
                Left $ Parse "name" "chars a-z, 0-9 and _" (colour Magenta (show d))
            | otherwise -> Right Header {
                width  = width, 
                height = height,
                frames = frames,
                name   = d
              }
    parseHelper w = Left $ Parse "header" "4 values" (show $ length w) 

colorChar :: Colored Char -> String
colorChar (Colored c s) = case c of
  Black   -> "\\x1b[30m" <> clean s
  Red     -> "\\x1b[31m" <> clean s
  Green   -> "\\x1b[32m" <> clean s
  Yellow  -> "\\x1b[33m" <> clean s
  Blue    -> "\\x1b[34m" <> clean s
  Magenta -> "\\x1b[35m" <> clean s
  Cyan    -> "\\x1b[36m" <> clean s
  White   -> "\\x1b[37m" <> clean s
  Transp  -> "\\x1b[30m" <> clean s
