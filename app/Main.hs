{-# Language MultiWayIf #-}
{-# Language TupleSections #-}
{-# Language LambdaCase #-}

module Main (main) where

-- import Lib
import System.Exit
import System.Process
import System.Environment

import Control.Applicative
import Control.Monad ((<=<), (>=>), guard, unless)
import Control.Arrow ((<<<), (>>>), (***), (&&&), (+++), (|||))

import Data.Bifunctor (first, second)
import Data.Maybe
import Data.Tuple
import Data.List
-- import System.IO
import Text.Read (readMaybe) 

import Types

infix 8 ...

unwrapNotated (Drawings x) = x
unwrapNotated (Script x) = x

number :: FilePath -> String -> [Marked String]
number f = zipWith (\x y -> Marked Mark {origin = f, line = x} y) [1..] . lines

defaultMods :: Modifiers
defaultMods = Modifiers {
  fps       = 0.2,
  directory = ".",
  message   = False,
  quiet     = False,
  check     = False
}

exitWithError :: Error -> IO ()
exitWithError = die . show

main = flip parseArgs defaultMods <$> getArgs >>= \case
  Left e -> exitWithError e
  Right (mods,target,args) ->
    (if message mods then getContents else pure "") >>= \stdIn ->
    concat . zipWith number args <$> mapM readFile args >>= (
      (
        cutSpace >=> parse >=>                                                  \x -> 
        (flip dependenciesOf target . map (name *** fmap (map unwrap))) x >>=   \d ->
        fromJust . find ((== target) . name . fst) <$> win [] d x >>=           \(header,gif) -> 
        pure . formatSh mods header stdIn $ map (renderer . chunksOf (width header)) gif 
      ) 
      >>> \case
        Left e -> exitWithError e
        Right x -> let name = directory mods <> "/" <> target <> ".sh" in
          putStr "\x1b[32;1mSuccess!\x1b[0m" >>
          unless (check mods) (
            writeFile name x >> 
            callCommand ("chmod +x " <> name) >>
            putStr ("\b\b Gif saved to " <> colour Cyan name <> ".")
          ) >>
          putStr "\n" >>
          unless (quiet mods) (callCommand name)
    )

parseArgs :: [String] -> Modifiers -> OrError (Modifiers, Name, [FilePath])
parseArgs ("-c":xs)     m = parseArgs xs m {check     = True}
parseArgs ("-q":xs)     m = parseArgs xs m {quiet     = True}
parseArgs ("-m":xs)     m = parseArgs xs m {message   = True}  
parseArgs ("-d":dir:xs) m = parseArgs xs m {directory = dir}  
parseArgs ("-f":fps:xs) m = case readMaybe fps >>= \n -> if n <= 0 then Nothing else pure n of 
  Nothing -> Left $ ArgError "The \x1b[33m-f\x1b[0m flag expected a positive \x1b[33mNUM\x1b[0m"
  Just x  -> parseArgs xs m {fps = 1.0 / x}
parseArgs ("-h":_)     _ = Left $ Help 
parseArgs (('-':x):_)  _ = Left . ArgError $ "Unknown argument '" <> colour Yellow ('-':x) <> "'"
parseArgs []           _ = Left $ ArgError "Args should end with \x1b[33mNAME <FILE>\x1b[0m"
parseArgs [_]          _ = Left $ ArgError "Args should end with \x1b[33mNAME <FILE>\x1b[0m"
parseArgs (name:args)  m = pure (m,name,args)

formatSh :: Modifiers -> Header -> String -> [String] -> String
formatSh m h d s = 
  "#!/bin/sh\n\n"
  <> (unlines . map ("# " <>) . lines) d
  <> case s of
    [x] ->
      "\nprintf '" 
      <> x
      <> "'\n"
    xs  -> let ht = height h in
      "\ndraw() {\n  printf \"\\033[" <> show ht <> "A\\r\\033[0J$1\"\n  sleep " 
      <> show (fps m) 
      <> "\n}\n"
      <> "printf '" <> take (ht * 2) (cycle "\\n") <> "\033[0m'\n" 
      <> "while true\ndo\n"
      <> concatMap (\x -> "  draw '" <> x <> "'\n") xs
      <> "done\n"

(>?) :: Mark -> OrToError a -> OrError a
(>?) x y = first ($ x) y

cons = (:)
append :: a -> [a] -> [a]
append  x y = y <> [x]
eq :: Eq a => a -> a -> Bool
eq = (==)

parse :: [Marked String] -> OrError (Map Header (Notated [Marked String])) 
parse []                = pure []
parse (Marked m l : xs) = m >? parseHeader l >>= \header -> 
  case words . unwrap <$> listToMaybe xs of
    Nothing      -> Left $ Custom "Header is lacking a body" m
    Just ["scr"] -> m >? getDelimiter (tail xs) "rcs" >>= \(script, other) -> 
                    cons (header,(Script script)) <$> parse other
    Just _       -> cons (header,(Drawings $ take len xs)) <$> parse (drop len xs)
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
    | w == good         -> pure ([],xs)
    | w == reverse good -> Left $ Delimiter (reverse good)
    | otherwise         -> first (cons x) <$> getDelimiter xs good   
  _   -> first (cons x) <$> getDelimiter xs good

cutSpace :: [Marked String] -> OrError [Marked String]
cutSpace []                    = Right []
cutSpace (x@(Marked m l) : xs) = case words l of 
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
        Just x  -> fmap (cons (target,x) . concat) . sequence . map (getDependencies (target:used)) $ x

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

solve :: EpicGifData -> Header -> Notated [Marked String] -> OrError (Header, Gif)
solve _ h (Drawings x) 
  = pure 
  . (h,)
  . map concat
  -- . map (zip (liftA2 (flip (,)) [1..height h] [1..width h]) . concat) 
  . chunksOf (height h) 
  . map (parseColorLine (width h) . unwrap) 
  $ x

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n x  = uncurry ((. chunksOf n) . cons) $ splitAt n x

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

clean = \case 
  '\\' -> "\\134"
  '\'' -> "\\047"
  '\n' -> "\\n"
  '%'  -> "%%"
  a    -> [a]

parseColorLine :: Int -> String -> [Colored Char]
parseColorLine = uncurry (zipWith Colored) . first (map charToColor) . swap ... splitAt
  where 
    charToColor = \case 
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

parseHeader :: String -> OrToError Header
parseHeader = parseHelper . words
  where
    parseHelper :: [String] -> OrToError Header
    parseHelper [a,b,c,d] = case readMaybe a of 
      Nothing     -> Left $ Parse "width" "an int" (colour Magenta (show a)) 
      Just 0      -> Left $ Custom "The header's width must be greater than 0"
      Just width  -> case readMaybe b of 
        Nothing     -> Left $ Parse "height" "an int" (colour Magenta (show b)) 
        Just 0      -> Left $ Custom "The header's height must be greater than 0"
        Just height -> case readMaybe c of 
          Nothing     -> Left $ Parse "Header's number of frames" "an int" (colour Magenta (show c)) 
          Just 0      -> Left $ Custom "The header must have at least one frame"
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
  Black   -> "\\033[30m" <> clean s
  Red     -> "\\033[31m" <> clean s
  Green   -> "\\033[32m" <> clean s
  Yellow  -> "\\033[33m" <> clean s
  Blue    -> "\\033[34m" <> clean s
  Magenta -> "\\033[35m" <> clean s
  Cyan    -> "\\033[36m" <> clean s
  White   -> "\\033[37m" <> clean s
  Transp  -> "\\033[30m" <> clean s
