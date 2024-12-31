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

maino = interact $ unlines . extractDependencies . lines

number :: FilePath -> String -> [Marked String]
number f = zipWith (\x y -> Marked Mark {origin = f, line = x} y) [1..] . lines

main = getArgs >>= \arguments -> case arguments of 
  []          -> putStr . show $ MissingArgs 0
  [_]         -> putStr . show $ MissingArgs 1
  (target:args) -> zipWith number args <$> mapM readFile args >>= (
      -- putStr 
      -- <<< 
      -- show ||| show
      -- <<< flip dependenciesOf target . map (name *** map unwrap)
      -- <=< parse
      -- <=< cutSpace
      -- <<< concat
      putStr <<< show ||| id <<< uwu target <<< concat
    )

uwu :: Name -> [Marked String] -> OrError String
uwu target = cutSpace >=> parse >=> lookupName >=>
  return . 
  (\x -> "echo -e '" <> x <> "'\n") . 
  -- concat .
  renderer . 
  map (uncurry parseColorLine) .   
  uncurry (\h -> map ((\x -> (width h,x)) . unwrap))
    where
      lookupName x = case find (eq target . name . fst) x of
        Nothing -> Left $ NoMatchingName target
        Just x  -> return x

(<$>?) :: (a -> OrToError b) -> Marked a -> OrError (Marked b) 
(<$>?) f (Marked m a) = case f a of
  Right x -> return (Marked m x)
  Left fn -> Left (fn m)

(=<<?) :: (a -> OrToError b) -> OrError (Marked a) -> OrError (Marked b)
(=<<?) _ (Left x) = Left x
(=<<?) f (Right x) = f <$>? x

(<=<?) :: (b -> OrToError c) -> (a -> OrToError b) ->  Marked a -> OrError (Marked c)
(<=<?) g f a =  case (f <$>? a) of
  Right x -> g <$>? x
  Left  x -> Left x

(>?) :: Mark -> OrToError a -> OrError a
(>?) x y = first ($ x) y

cons = (:)
append :: a -> [a] -> [a]
append  x y = y <> [x]
eq :: Eq a => a -> a -> Bool
eq = (==)

parse :: [Marked String] -> OrError (Map Header [Marked String])   
parse []           = return []
parse (Marked m l : xs) = m >? parseHeader l >>= 
  \header -> case words . unwrap <$> listToMaybe xs of
    Nothing         -> Left $ Custom "Header is lacking a body" m
    Just ["script"] -> m >? getDelimiter xs "end" [] >>= \(script, other) -> 
                         cons (header, script) <$> parse other
    Just _          -> cons (header, take len xs) <$> parse (drop len xs)
      where len = height header * frames header

-- if something has no dependencies it can be calculated
win :: EpicGifData -> Dependencies -> Map Name [Marked String] -> OrError EpicGifData
win uwu []          _ = pure uwu
win uwu ((n,[]):xs) d = solve uwu n (d ! n) >>= \s -> win (s:uwu) (second (filter (/= n)) <$> xs) d
win uwu (x:xs)      d = win uwu (append x xs) d

-- finds closing delimiter and returs up to and after it
-- getDelimiter :: Lines -> good -> [bad] -> OrError (inside, outside)
getDelimiter :: [Marked String] -> String -> [String] -> OrToError ([Marked String], [Marked String])
getDelimiter []       _    _   = Left Delimiter
getDelimiter (x:xs) good bad = case words $ unwrap x of
  [w] -> if 
    | w == good  -> return ([],xs)
    | elem w bad -> Left Delimiter
    | otherwise  -> first (cons x) <$> getDelimiter xs good bad   
  _   -> first (cons x) <$> getDelimiter xs good bad   

cutSpace :: [Marked String] -> OrError [Marked String]
cutSpace []                    = Right []
cutSpace (x@(Marked m l) : xs) = case words $ unwrap x of 
  []      -> cutSpace xs
  ["com"] -> m >? getDelimiter xs "moc" ["com"] >>= cutSpace . snd
  ["moc"] -> Left $ Custom "Unexpected moc" m
  _       -> cons x <$> cutSpace xs

unwrap :: Marked a -> a
unwrap (Marked _ a) = a  
    
dependenciesOf :: Map Name [String] -> Name -> OrError (Map Name [Name])
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

isValidName :: Name -> Bool
isValidName x = all ($ x) 
  [ flip notElem ["frame","end","com","moc","script"]
  , all (`elem` '_' : ['a'..'z'] <> ['0'..'9'])
  , any (`elem` ['a'..'z'])
  ]

extractDependencies :: [String] -> [Name]
extractDependencies x = case words . head $ x of
  ["script"] -> filter isValidName . map reverse . flip helper [] . unlines $ x
      where 
        helper :: String -> Name -> [Name]
        helper []     _ = []
        helper (x:xs) w = if x `elem` '_' : ['a'..'z'] <> ['1'..'9']
                          then helper xs (x:w)
                          else w : helper xs []
  _ -> []

(...) = (.).(.)

(!) = fromJust ... flip lookup

solve :: EpicGifData -> Name -> [Marked String] -> OrError (Name, Gif)
solve = undefined

-- TODO cut tai of ' ' <> colored \n
renderer :: [[Colored Char]] -> String
renderer = helper Transp . concat . map (append (Colored Transp '\n') . removeExtraSpaces)
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
        remove (a: as) = a:as


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

colorChar :: Colored Char -> String
colorChar c = case c of
  Colored Black   s -> "\\x1b[30m" <> clean s
  Colored Red     s -> "\\x1b[31m" <> clean s
  Colored Green   s -> "\\x1b[32m" <> clean s
  Colored Yellow  s -> "\\x1b[33m" <> clean s
  Colored Blue    s -> "\\x1b[34m" <> clean s
  Colored Magenta s -> "\\x1b[35m" <> clean s
  Colored Cyan    s -> "\\x1b[36m" <> clean s
  Colored White   s -> "\\x1b[37m" <> clean s
  Colored Transp  s -> "\\x1b[30m" <> clean s

parseHeader :: String -> OrToError Header
parseHeader = parseHelper . words
  where 
    parseHelper :: [String] -> OrToError Header
    parseHelper [a,b,c,d] = case readMaybe a of 
      Nothing     -> Left $ Parse "width"  "an int" (show a) 
      Just width  -> case readMaybe b of 
        Nothing     -> Left $ Parse "height" "an int" (show b)
        Just height -> case readMaybe c of 
          Nothing     -> Left $ Parse "number of frames" "an int" (show c) 
          Just frames -> if
            | not $ isValidName d -> Left $ Parse "name" "only chars a-z and _" (show d)
            | otherwise -> Right Header {
                width  = width, 
                height = height,
                frames = frames,
                name   = d
              }
    parseHelper w = Left $ Parse "header" "4 values" (show $ length w) 
