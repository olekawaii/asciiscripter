{-# Language MultiWayIf #-}
{-# Language TupleSections #-}
{-# Language LambdaCase #-}

module Main (main) where

-- import Lib
import System.Exit
import System.Process
import System.Environment

import Control.Applicative
import Control.Monad ((<=<), (>=>), guard, unless, when)
import Control.Arrow ((<<<), (>>>), (***), (&&&), (+++), (|||))

import qualified Data.Map.Strict as Map
import Data.Bifunctor (first, second)
import Data.Maybe
import Data.Tuple
import Data.List
-- import System.IO
import Text.Read (readMaybe) 

import Types

infix 8 ...

defaultMods :: Modifiers
defaultMods = Modifiers {
  fps       = 0.2,
  directory = ".",
  message   = False,
  quiet     = False,
  check     = False,
  text      = False
}

parseInt :: String -> String -> OrToError Int
parseInt x y = case readMaybe x of
  Just x  -> pure x
  Nothing -> Left $ Parse y  "an int" x

parseName :: String -> OrToError Name
parseName s = if isValidName s then pure s else Left $ Parse "name" "a valid name" s

unwrapNotated (Drawings x) = x
unwrapNotated (Script x)   = x

number :: FilePath -> String -> [Marked String]
number f = zipWith (\x y -> Marked File {origin = f, line = x} y) [1..] . lines

exitWithError :: Error -> IO ()
exitWithError = die . show

main = flip parseArgs defaultMods <$> getArgs >>= \case
  Left e -> exitWithError e
  Right (mods,target,args) ->
    concat . zipWith number args <$> mapM readFile args >>= (
      gigaParse target mods
      >>> \case 
        Left e -> exitWithError e
        Right (header,gif) -> 

          (if message mods then getContents else pure "") >>= \messageIn ->

          (
            if text mods 
            then fmap (
              flip mappend (cycle [[]]) . 
              map (map (Colored White)) . 
              take (height header) . 
              map (' ':) .
              lines
            ) getContents 
            else pure $ cycle [[]]
          ) >>= \textIn ->

          let 
            file = directory mods <> "/" <> target <> ".sh" 
            x    = 
              formatSh mods header messageIn . 
              map renderer $ 
              map (flip (zipWith mappend) textIn . chunksOf (width header)) gif
          in

          putStr "\x1b[32;1mSuccess!\x1b[0m\n" >>
          unless (check mods) (
            writeFile file x >> 
            callCommand ("chmod +x " <> file) >>
            putStr ("Gif saved to " <> colour Cyan file <> ".\n") >>
            unless (quiet mods) (
              putStr ("\r\x1b[0J" <> (if length gif > 1 then "\x1b[1A" else "") <> "\x1b[0m") >>
              callCommand file
            )
          )
    )

gigaParse :: Name -> Modifiers -> [Marked String] -> OrError (Header, Gif)
gigaParse target mods = 
  cutSpace >=> parse >=> \x -> 
  flip dependenciesOf target (map (first name) x) >>=
  \d -> fromJust . find ((== target) . name . fst) <$> win [] d x

maybeGuard :: (a -> Bool) -> a -> Maybe a
maybeGuard f a = if f a then pure a else Nothing

parseArgs ("-h":_)   _ = Left Help 
parseArgs (a:b:cs) m = case a of
  "-c"        -> parseArgs (b:cs) m {check     = True}
  "-q"        -> parseArgs (b:cs) m {quiet     = True}
  "-C"        -> parseArgs (b:cs) m {message   = True}  
  "-d"        -> parseArgs cs     m {directory = b   }  
  "-s"        -> parseArgs (b:cs) m {text      = True}
  "-f"        -> case readMaybe b >>= maybeGuard (>= 0) of 
    Nothing -> Left $ ArgError "The \x1b[33m-f\x1b[0m flag expected a positive \x1b[33mNUM\x1b[0m"
    Just x  -> parseArgs cs m {fps = 1 / x}
  ('-':'-':x) -> Left . ArgError $ "Unknown argument '" <> colour Yellow ('-':'-':x) <> "'"
  ['-',x]     -> Left . ArgError $ "Unknown argument '" <> colour Yellow ['-',x] <> "'"
  ('-':x)     -> parseArgs (map (cons '-' . pure) x <> (b:cs)) m
  name        -> 
    if isValidName name
    then pure (m,name,(b:cs)) 
    else Left . ArgError $ "The target arg " <> colour Magenta name <> " is not a valid name"
parseArgs _ _ = Left $ ArgError "Args should end with \x1b[33mNAME <FILE>\x1b[0m"

formatSh :: Modifiers -> Header -> String -> [String] -> String
formatSh m h d s = 
  "#!/bin/sh\n\n"
  <> (unlines . map ("# " <>) . lines) d
  <> case s of
    [x] ->
      "\nprintf '" <> x <> "'\n"
    xs  -> let ht = height h in
      -- "\ndraw() {\n  printf \"\\033[" <> show ht <> "A\\r\\033[0J$1\"\n  sleep " 
      "\ndraw() {\n  printf \"\\033[" <> show ht <> "A\\r$1\"\n  sleep " 
      <> show (fps m) 
      <> "\n}\n"
      <> "printf '" <> concat (replicate ht "\\n") <> "\\033[0m'\n" 
      <> "while true\ndo\n"
      <> concatMap (\x -> "  draw '" <> x <> "'\n") xs
      <> "done\n"

(>?) :: Mark -> OrToError a -> OrError a
(>?) x y = first ($ x) y

cons = (:)

append :: a -> [a] -> [a]
append  x y = y <> [x]

parse :: [Marked String] -> OrError (Map Header (Notated [Marked String])) 
parse []                = pure []
parse (ln@(Marked m@(File {line = lineNum}) l) : xs) = parseHeader ln >>= \header -> 
  case fmap words <$> listToMaybe xs of
    Nothing      -> Left $ Custom "Header is lacking a body" m
    Just (Marked d ["scr"]) -> d >? getDelimiter (tail xs) "rcs" >>= \(script, other) -> 
                    cons (header,(Script script)) <$> parse other
    Just (Marked d ["gif"]) -> d >? getDelimiter (tail xs) "fig" >>= \(gif,    other) -> 
                    cons (header,(Drawings gif)) <$> parse other
    Just _       -> Left $ Custom "expected a delimiter but found nothing" m
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
cutSpace (Marked m l : xs) = case words l of 
  []      -> cutSpace xs
  ["com"] -> m >? getDelimiter xs "moc" >>= cutSpace . snd
  ["moc"] -> Left $ BadDelimiter "moc" m
  _       -> cons (Marked m $ stripWhitespace l) <$> cutSpace xs

unwrap :: Marked a -> a
unwrap (Marked _ a) = a  
    
dependenciesOf :: Map Name (Notated [Marked String]) -> Name -> OrError (Map Name [Name])
dependenciesOf table = 
  fmap nub . 
  getDependencies [] . 
  Marked Arguments
  where
    getDependencies :: [Name] -> Marked Name -> OrError (Map Name [Name])
    getDependencies used (Marked m target) = 
      if elem target used 
      then Left $ Recursive target 
      else case extractDependencies <$> lookup target table of 
        Nothing -> Left $ NoMatchingName target m
        Just x  -> cons (target, map unwrap x) . concat <$> traverse (getDependencies (target:used)) x

extractDependencies :: Notated [Marked String] -> [Marked Name]
extractDependencies (Drawings _) = []
extractDependencies (Script x)   = 
  concat . 
  map format $ x
  where 
    format :: Marked String -> [Marked Name]
    format (Marked m x) = map (Marked m) deps
      where
        deps = helper x []

    helper :: String -> Name -> [Name]
    helper []     w = if isValidName w then [w] else []
    helper (x:xs) w = 
      if x `elem` legalNameChars
      then helper xs (w <> [x])
      else if isValidName w
           then w : helper xs []
           else helper xs []

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
  , not . null
  , all (`elem` legalNameChars)
  , flip elem ['a'..'z'] . head
  ]

(...) = (.).(.)

stripWhitespace :: String -> String
stripWhitespace = reverse . helper . reverse
  where 
    helper []       = []
    helper (' ':xs) = helper xs
    helper x        = x

solve :: EpicGifData -> Header -> Notated [Marked String] -> OrError (Header, Gif)
solve _ header (Drawings x) =
  -- pure . (header,) . map concat . chunksOf h . map (parseColorLine . unwrap) $ x
  if mod (length x) h /= 0 
  -- then Left . ReallyCustom $ 
    -- "gif's number of lines should be divisible by the header's height.\n You have " 
    -- <> show (length x) <> " lines." 
  then Left $ Custom 
    (
      "A gif's number of lines should be divisible by the header's height.\nYou have "
      <> show (length x) <> " lines." 
    )
    (mark header)
  else (header,) . concat <$> traverse validateStrings (chunksOf h x)
  where 
    validateStrings :: [Marked String] -> OrError [[Colored Char]]
    validateStrings [] = pure []
    validateStrings (x:xs) = map concat . chunksOf h . map parseColorLine . rearange <$> helper (x:xs) 
      where
        n = length . stripWhitespace $ unwrap x

        rearange :: [String] -> [String]
        rearange = concat . transpose . map (chunksOf (w * 2))

        helper :: [Marked String] -> OrError [String]
        helper [] = pure []
        helper ((Marked m x):xs) = let len = length x in
          if len /= n
          then Left $ Value "line" n len m
          else cons x <$> helper xs

    h = height header
    w = width  header

solve e h (Script x) = 
  formatFrames (fmap words <$> x) >>= traverse (traverse parse) >>= fmap (h,) . interpritCommands
  where
    formatFrames :: [Marked [String]] -> OrError [[Marked [String]]]
    formatFrames []               = pure []
    formatFrames (Marked m ("frame":as):xs) = 
      let (f,p) = findRestOfFrame xs in 
      case traverse readMaybe as :: Maybe [Int] of 
        Nothing -> Left $ Parse "command" "an Int" (concat as) m
        Just [x]   -> (f:) <$> formatFrames p
        Just [x,y] -> mappend (replicate (y - x + 1) f) <$> formatFrames p
      where
        findRestOfFrame :: [Marked [String]] -> ([Marked [String]], [Marked [String]])
        findRestOfFrame []                = ([],[])
        findRestOfFrame a@(Marked _ ("frame":_):_) = ([],a) 
        findRestOfFrame (Marked m x:xs)            = first (Marked m x :) $ findRestOfFrame xs
    
    parse :: Marked [String] -> OrError Command
    parse (Marked m (layer:command:xs)) = (m >?) $ parseInt layer "layer" >>= \layer -> case command of
      "DRAW" -> case xs of 
        [x,y,n] ->  
          parseInt x "x-coordinate" >>= \x ->
          parseInt y "y-coordinate" >>= \y ->
          pure $ Draw layer (x,y) (case find ((== n) . name . fst) e of
            Nothing -> error (show . map (name . fst) $ e )
            Just x -> x)
        x -> Left $ Value command 3 (length x)
      "SHIFT" -> case xs of
        [x,y] ->
          parseInt x "x-coordinate" >>= \x ->
          parseInt y "y-coordinate" >>= \y ->
          pure $ Shift layer x y
        x -> Left $ Value command 3 (length x)
      "SLOW" -> case xs of
        [num] -> 
          parseInt num "length of frame" >>= \num ->
          pure $ Slow layer num  
        x -> Left $ Value command 3 (length x)
      "REVERSE" -> case xs of
        [] -> pure $ Reverse layer
        x -> Left $ Value command 0 (length x)
      "CLEAR" -> case xs of
        [] -> pure $ Clear layer
        x -> Left $ Value command 0 (length x)
      "SKIP" -> case xs of 
        [x] -> 
          parseInt x "number of frames" >>= \num ->
          pure $ Skip layer num
        x -> Left $ Value command 1 (length x)
      _ -> Left (Parse "command" "Command" "Idk")

    interpritCommands :: [[Command]] -> OrError Gif
    interpritCommands coms = pure . map toFrame $ helper coms []
      where
        helper :: [[Command]] -> Map Int Layer -> [Map Int Layer]
        helper [] _ = [] 
        helper ([]:xs) sol = sol : helper xs (shift sol)
        helper ((x:as):xs) sol  = case x of
          Draw layer coord (header, gif) ->
            -- helper (as:xs) (changeGif sol layer . const $ map (formatFrame header) gif)
            helper (as:xs) $ 
            insertVal 
              layer 
              (Layer {coord = coord, gif = map (formatFrame header) gif, header = header}) 
              sol
          Shift layer x y -> 
            helper (as:xs) (changeGif sol layer (shiftAll x y <$>))
          Slow layer num ->
            helper (as:xs) (changeGif sol layer $ concat . map (replicate num))
          Reverse layer -> 
            helper (as:xs) (changeGif sol layer $ \case
              [] -> []
              (x:xs) -> x : reverse xs
            )
          Clear layer -> 
            helper (as:xs) (filter ((/= layer) . fst) sol)
          Skip layer num ->
            helper (as:xs) (changeGif sol layer (
                \x -> let d = num `toTake` length x in uncurry mappend . swap . splitAt d $ x
            ))
          where 
            changeGif :: Map Int Layer -> Int -> ([NumFrame] -> [NumFrame]) -> Map Int Layer
            changeGif g i f = case lookup i sol of
              Nothing -> g
              Just x  -> insertVal i x {gif = f $ yourGif i} g
            
            yourGif :: Int -> [NumFrame]
            yourGif l = gif . fromJust $ lookup l sol

            toTake :: Int -> Int -> Int
            toTake x y = if x <= y then y - x else toTake x (y - x)

        toFrame :: Map Int Layer -> Frame
        toFrame = unite . concat . map render . reverse . map snd . sortOn fst
          where 
            coords = liftA2 (flip (,))  [height h, height h -1 .. 1] [1..width h]

            render :: Layer -> Map Coordinate (Colored Char)
            render x = let (x_loc, y_loc) = coord x in 
              shiftAll x_loc y_loc (head $ gif x)

            unite :: Map Coordinate (Colored Char) -> [Colored Char]
            unite dict = uniteHelper  coords
              where 
                goodDict = filter (\(_,Colored x _) -> x /= Transp) dict
                uniteHelper [] = []
                uniteHelper (x:xs) = case lookup x goodDict of
                  Nothing -> Colored Transp ' ' : uniteHelper xs
                  Just a  -> a : uniteHelper xs

formatFrame :: Header -> Frame -> Map Coordinate (Colored Char)
formatFrame header gif = 
  liftA2
    (flip (,)) 
    [ height header -1, height header -2 .. 0] 
    [0 .. width header -1] 
  `zip`
  gif

shiftAll :: Int -> Int -> Map Coordinate a -> Map Coordinate a
shiftAll x y = map (first ((+ (x)) *** (+ (y))))


insertVal :: Eq a => a -> b -> Map a b -> Map a b
insertVal new val x = (new,val) : filter ((/= new) . fst) x

shift :: Map Int Layer -> Map Int Layer
shift = map (second (\b -> b {gif = rotate $ gif b})) 
  where 
    rotate [] = []
    rotate (x:xs) = xs <> [x]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n x  = uncurry ((. chunksOf n) . cons) $ splitAt n x

renderer :: [[Colored Char]] -> String
renderer = helper Transp . concatMap (append (Colored Transp '\n'))
-- renderer = helper Transp . concatMap (append (Colored Transp '\n') . removeExtraSpaces)
  where
    helper :: Color -> [Colored Char] -> String
    helper _        []                        = "" --"\\n"
    helper oldcolor (Colored color char : xs) = 
      if color == oldcolor || elem char " \n" || color == Transp
      then clean char <> helper oldcolor xs
      else colorChar (Colored color char) <> helper color xs
    
    removeExtraSpaces :: [Colored Char] -> [Colored Char]
    removeExtraSpaces = reverse . remove . reverse
      where
        remove (Colored _ ' ' : as) = remove as
        remove as = as

clean :: Char -> String
clean '\\' = "\\134"
clean '\'' = "\\047"
clean '\n' = "\\n"
clean '%'  = "%%"
clean a    = [a]

parseColorLine :: String -> [Colored Char]
parseColorLine x = uncurry (zipWith Colored) . first (map charToColor) . swap $ splitAt (length x `quot` 2) x
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

parseHeader :: Marked String -> OrError Header
parseHeader (Marked m s) = parseHelper $ words s
  where
    parseHelper :: [String] -> OrError Header
    parseHelper [a,b,c,d] = 
      m >? parseInt a "header's width"      >>= \width  ->
      m >? parseInt b "header's height"     >>= \height ->
      m >? parseInt c "header's framecount" >>= \frames ->
      m >? parseName d                      >>= \name   ->
        pure Header {
            width  = width, 
            height = height,
            frames = frames,
            name   = name,
            mark   = m
          }
    parseHelper w = Left $ Value "header" 4 (length w) m

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
