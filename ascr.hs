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
  Right (mods@Modifiers {target = t, directory = d, quiet = q, check = c, message = m}, files) ->
    (>>= evaluate) . newGiga (Marked Arguments t) mods . concat . zipWith number files <$> mapM readFile files >>= \case 
      Left e -> exitWithError e
      Right (I int) -> print int
      Right (G gif) -> 
        (if m then fmap pure getContents else pure Nothing) >>= \messageIn ->
        let
          file = d <> "/" <> t <> ".sh"
          (height, newGif) = changeFormat gif
          (fileSh, command) = new_formatShell mods height messageIn . pipeline $ newGif
        in
        putStr "\x1b[32;1mSuccess!\x1b[0m\n" >>
        unless c (
          print file >>
          writeFile file fileSh >> 
          callCommand ("chmod +x " <> file) >>
          putStrLn ("Gif saved to " <> colour Cyan file <> ".")
        ) >> 
        unless q (callCommand command)-- (callCommand command)

new_formatShell :: Modifiers -> Int  -> Maybe String -> [String] -> (ShellScript, ShellScript)
new_formatShell mods ht message renderedFrames = case renderedFrames of
  [frame] -> (gif, gif <> "; sleep 2" <> clear)
    where gif = comment <> "printf '" <> frame <> "'"
  frames  -> (intro <> loop <> body <> done, intro <> body <> clear)
    where 
      helper :: [String] -> String -> Float -> [String]
      helper [] _ i       = ["  sleep " <> show (frameTime mods * i) <> "\n # empty case\n"]
      helper (x:xs) old i = 
        if x == old 
        then helper xs old (i + 1)
        else let draw = "  draw '" <> x <> "'\n" in 
          if i > 0 
          then ("  sleep " <> show (frameTime mods * i) <> "\n") : draw : helper xs x 0.0
          else draw : helper xs x 0.0
      intro = comment <> "draw() {\n  printf \"\\033[" <> show ht <> "A\\r$1\"\n  sleep " <> 
        show (frameTime mods) <> "\n}\n" <> "printf '" <> concat (replicate ht "\\n") <> "\\033[0m'\n" 
      loop = "while true\ndo\n"
      body = concat (helper frames "" 0.0)
      done = "done"
  where
    comment = "#!/bin/sh\n" <> maybe "" (unlines . map ("# " <>) . lines) message <> "\n"
    clear = "\nprintf \x1b[" <> show ht <> "A\r\x1b[0J\x1b[0m"

getMark :: Marked x -> Mark
getMark (Marked m _) = m

defaultMods :: Modifiers
defaultMods = Modifiers {
  frameTime = 0.2,
  directory = ".",
  message   = False,
  quiet     = False,
  check     = False,
  text      = False,
  target    = "main"
}

-- TODO
changeFormat :: RealGif -> (Int, [[[Colored Char]]])
changeFormat x = (y_max - y_min + 1, map convertFrame x) 
  where 
    (xx,yy) = unzip $ map fst (head x)
    x_min = minimum xx
    x_max = maximum xx
    y_min = minimum yy
    y_max = maximum yy 
    chart = liftA2 (flip (,))  [y_max, y_max -1 .. y_min] [x_min..x_max]
    convertFrame :: Map Coordinate (Colored Char) -> [[Colored Char]]
    -- convertFrame a =  chunksOf (y_max - y_min) $ map lookupChar chart
    convertFrame a =  chunksOf (x_max - x_min + 1) $ map lookupChar chart
      where
        lookupChar want = case lookup want (filter (\(_, Colored m _) -> m /= Transp) a)of
          Just a  -> a
          Nothing -> Colored Transp ' '

parseInt :: String -> String -> OrToError Int
parseInt x y = case readMaybe x of
  Just x  -> pure x
  Nothing -> Left $ Parse y  "an int" x

parseName :: String -> OrToError Name
parseName s = if isValidName s then pure s else Left $ Parse "name" "a valid name" s

number :: FilePath -> String -> [Marked String]
number f = zipWith (\x y -> Marked File {origin = f, line = x, block = Nothing} y) [1..] . lines

exitWithError :: Error -> IO ()
exitWithError = die . show

newGiga target mods =
  cleanInput >=> new_parse >=> \x ->
  findDependencies x target >>=
  \d -> fromJust . lookup (unwrap target) <$> new_win builtinFns d x  

maybeGuard :: (a -> Bool) -> a -> Maybe a
maybeGuard f a = if f a then pure a else Nothing

parseArgs :: [String] -> Modifiers -> OrError (Modifiers, [FilePath])
parseArgs (a:as) m = case a of
  "-h"        -> Left Help 
  "-n"        -> getNext a as >>= \(b,cs) -> parseArgs cs     m {target    = b   }
  "-c"        -> parseArgs (as) m {check     = True}
  "-q"        -> parseArgs (as) m {quiet     = True}
  "-C"        -> parseArgs (as) m {message   = True}  
  "-d"        -> getNext a as >>= \(b,cs) -> parseArgs cs     m {directory = b   }  
  "-s"        -> parseArgs (as) m {text      = True}
  "-f"        -> getNext a as >>= \(b,cs) -> case readMaybe b >>= maybeGuard (>= 0) of 
    Nothing -> Left $ ArgError "The \x1b[33m-f\x1b[0m flag expected a positive \x1b[33mNUM\x1b[0m"
    Just x  -> parseArgs cs m {frameTime = 1 / x}
  ('-':'-':x) -> Left . ArgError $ "Unknown argument '" <> colour Yellow ('-':'-':x) <> "'"
  ['-',x]     -> Left . ArgError $ "Unknown argument '" <> colour Yellow ['-',x] <> "'"
  ('-':x)     -> parseArgs (map (cons '-' . pure) x <> (as)) m
  _           -> pure (m, a:as)
  where 
    getNext :: String -> [String] -> OrError (String, [String])
    getNext s []   = Left $ Custom (s <> " expected an argument") Arguments
    getNext _ [_]  = Left $ Custom ("Expected a list of files at the end") Arguments
    getNext _ (x:xs)    = pure (x, xs)
parseArgs _ _ = Left $ ArgError "Args should end with \x1b[33mNAME <FILE>\x1b[0m"

(>?) :: Mark -> OrToError a -> OrError a
(>?) x y = first ($ x) y

cons = (:)

append :: a -> [a] -> [a]
append  x y = y <> [x]

new_win :: Map Name Data -> Dependencies -> Map Name (NewHeader, [Marked String]) -> OrError (Map Name Data)
new_win uwu []          _ = pure uwu
new_win uwu ((n,[]):xs) d = 
  let (header@NewHeader {new_name = name}, str) = fromJust $ lookup n d in
  parseBlock header str uwu >>= \s -> 
  new_win ((name,s):uwu) (second (filter (/= n)) <$> xs) d
new_win uwu (x:xs)      d = new_win uwu (append x xs) d

unwrap :: Marked a -> a
unwrap (Marked _ a) = a  
    
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

findSimilarName :: Name -> [Name] -> Suggestion
findSimilarName name = Suggestion . listToMaybe . sortOn (cancel name)

cancel :: Name -> Name -> Int
cancel []     name = length name 
cancel (n:ns) name = let cut = rm n name in cancel ns cut + if cut == name then 1 else -1
  where 
    rm x []     = []
    rm x (y:ys) = if x == y then ys else y : rm x ys

applyFn :: Data -> Data -> OrError Data
applyFn Data {typeSigniture = Type t, currentName = name} _ = Left $ TypeMismatch name (Type t) None
applyFn 
  Data {typeSigniture = Fn a b, currentArgs = args, function = f, currentName = name} 
  arg@Data {typeSigniture = s, currentName = name2} = 
  if a == s
  then pure Data {
    currentName    = 
      name <> " " <> if length (words name2) > 1 then "(" <> name2 <> ")" else name2,
    typeSigniture  = b,
    currentArgs    = args <> [arg],
    function       = f
  } 
  else Left $ TypeMismatch name (Fn a b) None


evaluate :: Data -> OrError ReturnType
evaluate Data {typeSigniture = Fn _ _} = Left $ Custom "can't evaluate a function" None
evaluate Data {currentArgs = args, function = f} = pure $ f args

parseRealExpression want x = case evaluateExpression want x of
  Left x -> Left x
  Right (x,[]) -> Right x
  Right (Data {currentName = name, typeSigniture = t} ,_) -> Left $ TypeMismatch name t None
  
evaluateExpression :: Type -> [Data] -> OrError (Data, [Data])
evaluateExpression want [] = Left $ Custom "Missing arguments" None
evaluateExpression want (x@Data {typeSigniture = tp} : xs) = 
  if tp == want then pure (x,xs) else case tp of
    Type a -> pure (x,xs) --Left $ TypeMismatch "your mom" (tp) None -- pure (x,xs)
    Fn a b -> evaluateExpression a xs >>= \(arg, leftover) -> 
      -- let target = if typeSigniture arg == a then Fn a b else b in
      let target = if typeSigniture arg == a then a else b in
      applyFn x arg >>= evaluateExpression target . (:leftover) 

add :: Data
add = Data {
  currentName  = "add",
  typeSigniture = Fn (Type Int) (Fn (Type Int) (Type Int)),
  currentArgs = [],
  function = I . sum . map (\(I x) -> x) . map (fromRight . evaluate)
}

fromRight (Right x) = x

five = Data {
  currentName = "five",
  typeSigniture = Type Int,
  currentArgs = [],
  function = const (I 5)
}

parseTypeSigniture :: [String] -> Maybe (Type, [String])
parseTypeSigniture ("gif":xs) = pure (Type Giff, xs)
parseTypeSigniture ("int":xs) = pure (Type Int,  xs)
parseTypeSigniture ("fn":xs)  = 
  parseTypeSigniture xs >>= \(a,b) ->
  parseTypeSigniture b  >>= \(c,d) ->
  pure (Fn a c, d)
parseTypeSigniture _ = Nothing

-- uwu : fn int fn int int

-- newMain :: String -> IO ()
-- newMain = readFile >=> print . new_parse . lines

cleanInput :: [Marked String] -> OrError [Marked String]
cleanInput [] = pure []
cleanInput (Marked m s : xs) = case trim s of
  ""    -> cleanInput xs
  "---" -> findClosing xs >>= cleanInput
  "<o>" -> Left $ BadDelimiter "<o>" m
  _     -> (Marked m (stripWhitespace s) :) <$> cleanInput xs
  where
    findClosing [] = pure []
    findClosing (Marked m a : as) = case trim a of
      "<o>" -> pure as
      "---" -> Left $ BadDelimiter "---" m
      _     -> findClosing as
    

trim :: String -> String
trim = reverse . trimhelper . reverse . trimhelper
  where
    trimhelper [] = []
    trimhelper (' ':xs) = trimhelper xs
    trimhelper x = x

builtinFns :: Map Name Data 
builtinFns = [
    ("move"        , movef        ),
    ("reverse"     , reversef     ),
    ("skip"        , skipf        ),
    ("join"        , joinf        ),
    ("slow"        , slowf        ),
    ("null"        , nullf        ),
    ("seq"         , seqf         ),
    ("take"        , takef        ),
    ("frame_count" , frame_countf ),
    ("dye"         , dyef         ),
    ("black"       , black        ),
    ("red"         , red          ),
    ("green"       , green        ),
    ("yellow"      , yellow       ),
    ("blue"        , blue         ),
    ("magenta"     , magenta      ),
    ("cyan"        , cyan         ),
    ("white"       , white        )
  ]
  where 
    
    movef = Data {
      currentName   = "move",
      typeSigniture = Fn (Type Int) (Fn (Type Int) (Fn (Type Giff) (Type Giff))),
      currentArgs   = [],
      function      = \[a, b, c] -> 
        let 
          Right (I x) = evaluate a 
          Right (I y) = evaluate b
          Right (G z) = evaluate c
        in
        G $ map (map (\((f,g),thing) -> ((f + x, g + y), thing))) z
    }

    reversef = Data {
      currentName   = "reverse",
      typeSigniture = Fn (Type Giff) (Type Giff),
      currentArgs   = [],
      function      = \[a] ->
        let Right (G x) = evaluate a in
        G $ reverse x
    }

    skipf = Data {
      currentName   = "skip",
      typeSigniture = Fn (Type Int) (Fn (Type Giff) (Type Giff)),
      currentArgs   = [],
      function      = \[a,b] ->
        let 
          Right (I x) = evaluate a 
          Right (G y) = evaluate b
          
          toTake :: Int -> Int -> Int
          toTake x y = if x <= y then x else toTake (x - y) y
          
          (fst, snd)  = splitAt (toTake x (length y)) y
        in G $ snd <> fst
    }

    joinf = Data {
      currentName   = "skip",
      typeSigniture = Fn (Type Giff) (Fn (Type Giff) (Type Giff)),
      currentArgs   = [],
      function      = \[a,b] ->
        let
          Right (G x) = evaluate a
          Right (G y) = evaluate b

          frames = length x `lcm` length y in 
        G . map (uncurry mappend) . take frames $ zip (cycle x) (cycle y)
    }

    nullf = Data {
      currentName   = "null",
      typeSigniture = Type Giff,
      currentArgs   = [],
      function      = \_ ->
        G [[((1,1), Colored Transp ' ')]]
    }

    slowf = Data {
      currentName   = "slow",
      typeSigniture = Fn (Type Int) (Fn (Type Giff) (Type Giff)),
      currentArgs   = [],
      function      = \[a,b] ->
        let 
          Right (I x) = evaluate a
          Right (G y) = evaluate b
        in G . concat . map (replicate x) $ y
    }    

    seqf = Data {
      currentName   = "seq",
      typeSigniture = Fn (Type Giff) (Fn (Type Giff) (Type Giff)),
      currentArgs   = [],
      function      = \[a,b] ->
        let
          Right (G x) = evaluate a
          Right (G y) = evaluate b
        in 
        G $ x <> y
    }

    takef = Data {
      currentName   = "take",
      typeSigniture = Fn (Type Int) (Fn (Type Giff) (Type Giff)),
      currentArgs   = [],
      function      = \[a,b] ->
        let 
          Right (I x) = evaluate a
          Right (G y) = evaluate b
        in 
        G . take x $ cycle y
    }

    frame_countf = Data {
      currentName   = "frame_count",
      typeSigniture = Fn (Type Giff) (Type Int),
      currentArgs   = [],
      function      = \[a] ->
        let
          Right (G x) = evaluate a
        in I $ length x
    }

    dyef = Data {
      currentName   = "dye",
      typeSigniture = Fn (Type Colour) (Fn (Type Giff) (Type Giff)),
      currentArgs   = [],
      function      = \[a,b] ->
        let 
          Right (C x) = evaluate a
          Right (G y) = evaluate b
        in G $ map (map (\(coord, Colored _ char) -> (coord, Colored x char))) y
    }

    createColor :: Color -> Data
    createColor x = Data {
      currentName   = show x,
      typeSigniture = Type Colour,
      currentArgs   = [],
      function      = const (C x)
    }

    black   = createColor Black   
    red     = createColor Red     
    green   = createColor Green   
    yellow  = createColor Yellow  
    blue    = createColor Blue    
    magenta = createColor Magenta 
    cyan    = createColor Cyan    
    white   = createColor White

parseBlock :: NewHeader -> [Marked String] -> Map Name Data -> OrError Data
parseBlock header all@(x:xs) table = 
  let 
    fun = case traverse readMaybe (words (unwrap x)) of
      Just [a, b, c] -> parseGif header a b c xs
      Just _         -> Left $ Custom "incorrect art header" (block_mark header)
      Nothing        -> parseScript header table (concat . map (words . unwrap) $ all) 
  in fun >>= \fn -> pure Data {
    currentName = new_name header,
    typeSigniture = typeSig header,
    currentArgs = [],
    function = fn
  }


parseGif :: NewHeader -> Int -> Int -> Int -> [Marked String] -> OrError ([Data] -> ReturnType)
parseGif header w h f lns = 
  if mod (length lns) h /= 0 
  then Left $ Custom 
    (
      "A gif's number of lines should be divisible by the header's height.\nYou have "
      <> show (length lns) <> " lines." 
    )
    (block_mark header)
  else concat <$> traverse validateStrings (chunksOf h lns) >>= \gif -> 
    if length gif == f
      then pure $ \_ -> G $ 
            map (liftA2 (flip (,))  [h, h -1 .. 1] [1..w] `zip`) gif
      else Left $ Value "script's number of frames" "frames" f (length gif) (block_mark header) 
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

parseScript :: NewHeader -> Map Name Data -> [String] -> OrError ([Data] -> ReturnType)
parseScript NewHeader {typeSig = tp} table xs = 
  -- TODO test if argument types match up
  helperParseScript xs >>= \newTable -> pure $ \args -> 
      let 
      matcher x = case x of
        Left i  -> args !! (i - 1)
        Right x -> x
      in
      case parseRealExpression (result tp) $ map matcher newTable of
        Left e  -> error (show e)
        Right e -> case evaluate e of
          Left  e -> error (show e)
          Right x -> x
    where
      helperParseScript :: [String] -> OrError [Either Int Data]
      helperParseScript [] = pure []
      helperParseScript (('$':num):xs) = case readMaybe num of
        Nothing -> Left $ Custom "$ must be preceded by a number" None
        Just x  -> 
          if x > numberOfArgs tp || x < 1
          then Left $ Custom "index is greater than the number of arguments" None 
          else (Left x :) <$> helperParseScript xs
      helperParseScript (x:xs) = 
        if all (`elem` nums) x 
        then 
          let 
            d = Data {
              currentName   = x,
              typeSigniture = Type Int,
              currentArgs   = [],
              function      = const (I (read x))
            }
          in (Right d :) <$> helperParseScript xs
          
        else
          case lookup x table of
            Nothing -> Left $ Custom "variable not in scope" None
            Just x  -> (Right x :) <$> helperParseScript xs


numberOfArgs :: Type -> Int
numberOfArgs (Type _) = 0
numberOfArgs (Fn _ b) = 1 + numberOfArgs b

result :: Type -> Type
result (Type x) = Type x
result (Fn _ b) = result b

findDependencies :: Map Name (NewHeader, [Marked String]) -> Marked Name -> OrError (Map Name [Name])
findDependencies table = fmap nub . getDependencies []
  where 
    getDependencies :: [Name] -> Marked Name -> OrError (Map Name [Name])
    getDependencies used (Marked m target) =
      if elem target (map fst builtinFns) then pure [] else
      if elem target used
      then Left $ Recursive target (reverse $ target : used) 
      else case extract . snd <$> lookup target table :: Maybe [Marked String] of 
        Nothing -> Left $ NoMatchingName target (findSimilarName target (map fst table)) m
        Just x  -> cons (target, map unwrap x) . concat <$> traverse (getDependencies (target:used)) x
        
    extract :: [Marked String] -> [Marked Name]
    extract allah@(Marked m x : xs) = 
      if head x `elem` nums
      then [] 
      else helper . concat . map (\(Marked m x) -> map (Marked m) (words x)) $ allah
        where
          helper :: [Marked String] -> [Marked String]
          helper = filter (\(Marked m x) -> (x `notElem` map fst builtinFns) && not (all (`elem` ('$':nums)) x))

new_parse :: [Marked String] -> OrError (Map Name (NewHeader, [Marked String]))
new_parse [] = pure []
new_parse all@(Marked m x : xs) = 
  find_leftover xs >>= \(inside, other) -> ((new_name header, (header, inside)) :) <$> new_parse other
  where 
    header = parse_header $ words x

    find_leftover :: [Marked String] -> OrError ([Marked String],[Marked String])
    find_leftover [] = Left $ Parse "code block" "end" "EOF" m
    find_leftover (Marked m a : as) = case trim a of
      "end" -> pure ([],as)
      _     -> fmap (first (Marked m a :)) $ find_leftover as

    parse_header (x:xs) = NewHeader {
      new_name   = init x,
      typeSig    = fst $ fromJust $ parseTypeSigniture xs,
      block_mark = m
    }

nums = ['0'..'9']

checkTypes :: Mark -> Name -> Type -> Map Type Name -> OrError (Name, Map Type Name)
--    x     mark currentName currentType [next]
checkTypes m name (Type a) idc = pure (name, idc) 
checkTypes m name f@(Fn a b) other@((t,n):xs) = 
  if f == t then pure (name <> surround n, xs) else case t of
    Type _ -> Left $ TypeMismatch name a m
    Fn x y -> 
        if not (isPossible a t) then Left $ TypeMismatch name a m else
        checkTypes m name a other >>= \(arg, leftover) -> 
        checkTypes m (name <> surround arg) b leftover 
  -- if t == want then pure (xs) else case t of
  --   Type a -> Left $ TypeMismatch name want m
  --   Fn a b -> 

surround :: String -> String
surround x = case words x of
  [_] -> x
  _   -> " (" <> x <> ")"

isPossible :: Type -> Type -> Bool
isPossible w f@(Fn a b) = w == f || isPossible w b
isPossible w got        = w == got
  
-- checkTypes want [] = Left $ Custom "Missing arguments" None
-- checkTypes want (x@Data {typeSigniture = tp} : xs) = 
--   if tp == want then pure (x,xs) else case tp of
--     Type a -> pure (x,xs) --Left $ TypeMismatch "your mom" (tp) None -- pure (x,xs)
--     Fn a b -> checkTypes a xs >>= \(arg, leftover) -> 
--       -- let target = if typeSigniture arg == a then Fn a b else b in
--       let target = if typeSigniture arg == a then a else b in
--       applyFn x arg >>= checkTypes target . (:leftover) 



new_evaluate :: Data -> OrToError ReturnType
new_evaluate Data {typeSigniture = Fn _ _} = Left $ Custom "can't new_evaluate a function"
new_evaluate Data {currentArgs = args, function = f} = pure $ f args

new_parseRealExpression want x = case new_evaluateExpression want x of
  RealError x -> Left x
  Success (x,[]) -> Right x
  Func f -> Left $ Custom "TypeMismatch in the first argument"

data ComplexError = RealError (Mark -> Error) | Func (Name -> Mark -> Error) | Success (Data, [Data]) 

-- new_evaluateExpression :: Type -> [Data] -> ComplexError
-- new_evaluateExpression want [] = Func (\name -> TypeMismatch name want)
-- new_evaluateExpression (Type a) (x@Data {typeSigniture = Type b} : xs) = 
--   if a == b then Success (x,xs) else Func (\name -> TypeMismatch name (Type a))  
-- new_evaluateExpression want (x@Data {typeSigniture = tp} : xs) = 
--   if tp == want then Success (x,xs) else case tp of
--     Type a -> Func (flip TypeMismatch want) -- pure (x,xs)
--     Fn a b -> case new_evaluateExpression a xs of
--       RealError x             -> RealError x
--       Func f                  -> RealError (f $ currentName x)
--       Success (arg, leftover) -> 
--       -- let target = if typeSigniture arg == a then Fn a b else b in
--         let target = if typeSigniture arg == a then a else b in
--         case applyFn x arg of
--           Left x -> error "new_eval"
--           Right d -> case new_evaluateExpression target (d:leftover) of 
--             RealError x             -> RealError x
--             Func f                  -> RealError (f $ currentName d)
--             Success (arg, leftover) -> Success (arg, leftover)
new_evaluateExpression :: Type -> [Data] -> ComplexError
new_evaluateExpression want [] = Func (\name -> TypeMismatch name want)
new_evaluateExpression (Type a) (x@Data {typeSigniture = Type b} : xs) = 
  if a == b then Success (x,xs) else Func (\name -> TypeMismatch name (Type a))  
new_evaluateExpression want (x@Data {typeSigniture = tp} : xs) = 
  if tp == want then Success (x,xs) else case tp of
    Type a -> Func (flip TypeMismatch want) -- pure (x,xs)
    Fn a b -> case new_evaluateExpression a xs of
      RealError x             -> RealError x
      Func f                  -> RealError (f $ currentName x)
      Success (arg, leftover) -> 
        case applyFn x arg of
          Left x -> error "new_eval"
          Right d -> case new_evaluateExpression want (d:leftover) of 
            RealError x             -> RealError x
            Func f                  -> RealError (f $ currentName d)
            Success (arg, leftover) -> Success (arg, leftover)

dangerous_evaluateExpression :: Type -> [Data] -> (Data, [Data])
dangerous_evaluateExpression (Type a) (x@Data {typeSigniture = Type _} :xs) = (x,xs)  
dangerous_evaluateExpression want (x@Data {typeSigniture = tp} : xs) = 
  if tp == want then (x,xs) else
    let 
      (arg, leftover ) = dangerous_evaluateExpression a xs 
      (Fn a _)         = tp
    in
        dangerous_evaluateExpression want (dangerous_applyFn x arg : leftover) 
        
dangerous_applyFn :: Data -> Data -> Data
dangerous_applyFn 
  Data {typeSigniture = Fn a b, currentArgs = args, function = f, currentName = name} 
  arg@Data {currentName = name2} = 
  Data {
    currentName    = 
      name <> " " <> if length (words name2) > 1 then "(" <> name2 <> ")" else name2,
    typeSigniture  = b,
    currentArgs    = args <> [arg],
    function       = f
  } 
  
-- new_parseRealExpression want x = case new_evaluateExpression want x of
--   Success (x,[]) -> Right x
--   Func f -> Left $ Custom "TypeMismatch in the first argument"
