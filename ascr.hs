{-# Language MultiWayIf, TupleSections, LambdaCase #-}

module Main (main) where

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
import Text.Read (readMaybe) 

import Types

infix 8 ...

main = flip parseArgs defaultMods <$> getArgs >>= \case
  Left e -> exitWithError e
  Right (mods,target,args) ->
    gigaParse target mods . concat . zipWith number args <$> mapM readFile args >>= \case 
      Left e -> exitWithError e
      Right (header,gif) -> 

        (if message mods then getContents else pure "") >>= \messageIn ->

        let
          file = directory mods <> "/" <> target <> ".sh"
          (tp, x) = formatSh mods header messageIn . pipeline $ map (chunksOf $ width header) gif
        in

        putStr "\x1b[32;1mSuccess!\x1b[0m\n" >>
        unless (check mods) (
          writeFile file x >> 
          callCommand ("chmod +x " <> file) >>
          putStrLn ("Gif saved to " <> colour Cyan file <> ".") >>
          unless (quiet mods) (successGif tp file (height header))
        )

successGif :: OutputFile -> FilePath -> Int -> IO ()
successGif fileType file height = callCommand $ case fileType of
  Image ->
    file 
    <> "&& sleep 5 && printf \x1b[" 
    <> show height
    <> "A\r\x1b[0J\x1b[0m"
  Gif   ->
    "cat " 
    <> file
    <> " | grep -v '\\\\\\\\' | sh && printf \x1b[" 
    <> show height
    <> "A\r\x1b[0J\x1b[0m"

getMark :: Marked x -> Mark
getMark (Marked m _) = m

defaultMods :: Modifiers
defaultMods = Modifiers {
  frameTime = 0.2,
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

gigaParse :: Name -> Modifiers -> [Marked String] -> OrError (Header, Gif)
gigaParse target mods = 
  cutSpace >=> parse >=> \x -> 
  flip dependenciesOf target (map (first name) x) >>=
  \d -> fromJust . find ((== target) . name . fst) <$> win [] d x

maybeGuard :: (a -> Bool) -> a -> Maybe a
maybeGuard f a = if f a then pure a else Nothing

parseArgs ("-h":_) _ = Left Help 
parseArgs (a:b:cs) m = case a of
  "-c"        -> parseArgs (b:cs) m {check     = True}
  "-q"        -> parseArgs (b:cs) m {quiet     = True}
  "-C"        -> parseArgs (b:cs) m {message   = True}  
  "-d"        -> parseArgs cs     m {directory = b   }  
  "-s"        -> parseArgs (b:cs) m {text      = True}
  "-f"        -> case readMaybe b >>= maybeGuard (>= 0) of 
    Nothing -> Left $ ArgError "The \x1b[33m-f\x1b[0m flag expected a positive \x1b[33mNUM\x1b[0m"
    Just x  -> parseArgs cs m {frameTime = 1 / x}
  ('-':'-':x) -> Left . ArgError $ "Unknown argument '" <> colour Yellow ('-':'-':x) <> "'"
  ['-',x]     -> Left . ArgError $ "Unknown argument '" <> colour Yellow ['-',x] <> "'"
  ('-':x)     -> parseArgs (map (cons '-' . pure) x <> (b:cs)) m
  name        -> 
    if isValidName name
    then pure (m,name,(b:cs)) 
    else Left . ArgError $ "The target arg " <> colour Magenta name <> " is not a valid name"
parseArgs _ _ = Left $ ArgError "Args should end with \x1b[33mNAME <FILE>\x1b[0m"

formatSh :: Modifiers -> Header -> String -> [String] -> (OutputFile, String)
formatSh m h d [x] = (Image,) $
      "#!/bin/sh\n\n"
      <> (unlines . map ("# " <>) . lines) d
      <> "\nprintf '" <> x <> "'\n"
formatSh m h d xs = let ht = height h in (Gif,) $
      "#!/bin/sh\n\n"
      <> (unlines . map ("# " <>) . lines) d
      <> "\ndraw() {\n  printf \"\\033[" <> show ht <> "A\\r$1\"\n  sleep " 
      <> show (frameTime m) 
      <> "\n}\n"
      <> "printf '" <> concat (replicate ht "\\n") <> "\\033[0m'\n" 
      <> "while true #\\\\\ndo #\\\\\n"
      -- <> (concat . rotate . init $ helper xs "" 0.0)
      <> (concat $ helper xs "" 0.0)
      <> "done #\\\\"
    where 
      helper :: [String] -> String -> Float -> [String]
      helper [] _ i       = ["  sleep " <> show (frameTime m * i) <> "\n # empty case\n"]
      helper (x:xs) old i = 
        if x == old 
        then helper xs old (i + 1)
        else let draw = "  draw '" <> x <> "'\n" in 
          if i > 0 
          then ("  sleep " <> show (frameTime m * i) <> "\n") : draw : helper xs x 0.0
          else draw : helper xs x 0.0


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
dependenciesOf table = fmap nub . getDependencies [] . Marked Arguments
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
  -- else (header,) . concat <$> traverse validateStrings (chunksOf h x)
  else concat <$> traverse validateStrings (chunksOf h x) >>= \gif -> 
    if length gif == frames header
          then pure $ (header,) gif
          else Left $ Value "script's number of frames" "frames" (frames header) (length gif) (mark header) 
  where 
    validateStrings :: [Marked String] -> OrError [[Colored Char]]
    validateStrings [] = pure []
    validateStrings (x:xs) = firstStrLen >>= \lnlen -> 
      map concat . chunksOf h . map parseColorLine . rearange <$> helper (x:xs) lnlen
      where
        firstStrLen = let lnlen = length . stripWhitespace $ unwrap x in
          if lnlen `mod` w == 0
          then pure lnlen
          else Left $ Custom 
            (
              "A gif's chars per line should be divisible by the header's width.\nYou have "
              <> show lnlen <> " chars." 
            ) (getMark x)

        rearange :: [String] -> [String]
        rearange = concat . transpose . map (chunksOf (w * 2))

        helper :: [Marked String] -> Int -> OrError [String]
        helper [] _ = pure []
        helper ((Marked m x):xs) n = let len = length x in
          if len /= n
          then Left $ Value "line" "characters" n len m
          else cons x <$> helper xs n

    h = height header
    w = width  header

solve e header (Script x) = 
  formatFrames (fmap words <$> x) >>= 
  traverse (traverse parse)       >>=
  -- >>= fmap (h,) . interpritCommands
  interpritCommands               >>=
  \gif -> if length gif == frames header
          then pure $ (header,) gif
          else Left $ Value "script's number of frames" "frames" (frames header) (length gif) (mark header) 
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
        x -> Left $ Value command "args" 3 (length x)
      "SHIFT" -> case xs of
        [x,y] ->
          parseInt x "x-coordinate" >>= \x ->
          parseInt y "y-coordinate" >>= \y ->
          pure $ Shift layer x y
        x -> Left $ Value command "args" 3 (length x)
      "SLOW" -> case xs of
        [num] -> 
          parseInt num "length of frame" >>= \num ->
          pure $ Slow layer num  
        x -> Left $ Value command "args" 3 (length x)
      "REVERSE" -> case xs of
        [] -> pure $ Reverse layer
        x -> Left $ Value command "args" 0 (length x)
      "CLEAR" -> case xs of
        [] -> pure $ Clear layer
        x -> Left $ Value command "args" 0 (length x)
      "SKIP" -> case xs of 
        [x] -> 
          parseInt x "number of frames" >>= \num ->
          pure $ Skip layer num
        x -> Left $ Value command "args" 1 (length x)
      "FREEZE" -> case xs of
        [] -> pure $ Freeze layer
        x -> Left $ Value command "args" 0 (length x)
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
          Freeze layer -> 
            helper (as:xs) (changeGif sol layer $ \case
              [] -> []
              (x:xs) -> [x]
            )
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
            toTake x y = if x <= y then x else toTake (x - y) y

        toFrame :: Map Int Layer -> Frame
        toFrame = unite . concat . map render . reverse . map snd . sortOn fst
          where 
            coords = liftA2 (flip (,))  [height header, height header -1 .. 1] [1..width header]

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

rotate [] = []
rotate (x:xs) = xs <> [x]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n x  = uncurry ((. chunksOf n) . cons) $ splitAt n x

renderer :: [[Colored Char]] -> String
renderer = helper Transp . concatMap (append (Colored Black '\n'))
-- renderer = helper Transp . concatMap (append (Colored Transp '\n') . removeExtraSpaces)
  where
    helper :: Color -> [Colored Char] -> String
    helper _        []                        = "" --"\\n"
    helper oldcolor (Colored color char : xs) = if
      | color == Transp || char == ' '     -> ' ' : helper oldcolor xs
      | color == oldcolor || char == '\n'  -> clean char <> helper oldcolor xs
      | otherwise                          -> colorChar (Colored color char) <> helper color xs
    
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


pipeline :: [[[Colored Char]]] -> [String]
pipeline = map renderer . reduce3 . reduce2 . reduce1

-- turns all Transparent chars to spaces
reduce1 :: [[[Colored Char]]] -> [[[Colored Char]]]
reduce1 = map (map (map toSpace))
  where 
    toSpace x@(Colored color char) = 
      if color == Transp || elem char " \n"
      then Colored Black ' '
      else x

-- trims off unused space from the ends of lines. Only if doesn't break other frames
reduce2 :: [[[Colored Char]]] -> [[[Colored Char]]]
reduce2 []  = []
reduce2 [x] = [map removeExtraSpaces x]
reduce2 x   = 
  (\ns -> map ((zipWith (\a b -> take a b) ns)) x) . 
  map maximum . 
  transpose . 
  map (map (length . removeExtraSpaces)) $ 
  x

-- if all frames are the same, reduces it to an Image
reduce3 :: [[[Colored Char]]] -> [[[Colored Char]]]
reduce3 [] = []
reduce3 (x:xs) = if all (== x) xs then [x] else (x:xs)

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
parseHeader (Marked m s) = m >? parseHelper (words s)
  where
    parseHelper :: [String] -> OrToError Header
    parseHelper [a,b,c,d] =
      parseInt a "header's width"      >>= \width  ->
      parseInt b "header's height"     >>= \height ->
      parseInt c "header's framecount" >>= \frames ->
      parseName d                      >>= \name   ->
        pure Header {
            width  = width, 
            height = height,
            frames = frames,
            name   = name,
            mark   = m
          }
    parseHelper w = Left $ Value "header" "values" 4 (length w)

colorChar :: Colored Char -> String
colorChar (Colored c s) = "\\033[3" <> case c of
    Black   -> "0"
    Red     -> "1"
    Green   -> "2"
    Yellow  -> "3"
    Blue    -> "4"
    Magenta -> "5"
    Cyan    -> "6"
    White   -> "7"
    Transp  -> "0"
  <> "m" <> clean s
