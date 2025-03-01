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
    (evaluate <=< newGiga (Marked Arguments t) mods . concat . zipWith number files) <$> mapM readFile files >>= \case 
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
      helper [] _ 0.0     = [""]    
      helper [] _ i       = ["  sleep " <> show (frameTime mods * i)]
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
  cleanInput >=> parseChunks >=> \x ->
  findDependencies x target >>=
  \d -> fromJust . lookup (unwrap target) <$> win builtinFns d x  

maybeGuard :: (a -> Bool) -> a -> Maybe a
maybeGuard f a = if f a then pure a else Nothing

parseArgs :: [String] -> Modifiers -> OrError (Modifiers, [FilePath])
parseArgs (a:as) m = case a of
  "-h"        -> Left Help 
  "-n"        -> getNext a as >>= \(b,cs) -> parseArgs cs     m {target    = b   }
  "-c"        -> parseArgs as m {check     = True}
  "-q"        -> parseArgs as m {quiet     = True}
  "-C"        -> parseArgs as m {message   = True}  
  "-d"        -> getNext a as >>= \(b,cs) -> parseArgs cs     m {directory = b   }  
  "-s"        -> parseArgs as m {text      = True}
  "-f"        -> getNext a as >>= \(b,cs) -> case readMaybe b >>= maybeGuard (>= 0) of 
    Nothing -> Left $ ArgError "The \x1b[33m-f\x1b[0m flag expected a positive \x1b[33mNUM\x1b[0m"
    Just x  -> parseArgs cs m {frameTime = 1 / x}
  ('-':'-':x) -> Left . ArgError $ "Unknown argument '" <> colour Yellow ('-':'-':x) <> "'"
  ['-',x]     -> Left . ArgError $ "Unknown argument '" <> colour Yellow ['-',x] <> "'"
  ('-':x)     -> parseArgs (map (cons '-' . pure) x <> as) m
  _           -> pure (m, a:as)
  where 
    getNext :: String -> [String] -> OrError (String, [String])
    getNext s []   = Left $ Custom (s <> " expected an argument") Arguments
    getNext _ [_]  = Left $ Custom "Expected a list of files at the end" Arguments
    getNext _ (x:xs)    = pure (x, xs)
parseArgs _ _ = Left $ ArgError "Args should end with \x1b[33mNAME <FILE>\x1b[0m"

(>?) :: Mark -> OrToError a -> OrError a
(>?) x = first ($ x)

cons = (:)

append :: a -> [a] -> [a]
append  x y = y <> [x]

win :: Map Name Data -> Dependencies -> Map Name (NewHeader, [Marked String]) -> OrError (Map Name Data)
win uwu []          _ = pure uwu
win uwu ((n,[]):xs) d = 
  let (header@NewHeader {new_name = name}, str) = fromJust $ lookup n d in
  parseBlock header str uwu >>= \s -> 
  win ((name,s):uwu) (second (filter (/= n)) <$> xs) d
win uwu (x:xs)      d = win uwu (append x xs) d

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
shiftAll x y = map (first ((+ x) *** (+ y)))

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
  (\ns -> map (zipWith take ns) x) . 
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

evaluate :: Data -> OrError ReturnType
evaluate Data {typeSigniture = Fn _ _} = Left $ Custom "can't evaluate a function" None
evaluate Data {currentArgs = args, function = f} = pure $ f args

fromRight (Right x) = x

parseType :: [String] -> OrToError Type
parseType x = parseTypeSigniture x >>= \case
  (t,[]) -> pure t
  _      -> Left $ Custom "Trailing words after the type signiture"
parseTypeSigniture :: [String] -> OrToError (Type, [String])
parseTypeSigniture ("gif":xs) = pure (Type Giff, xs)
parseTypeSigniture ("int":xs) = pure (Type Int,  xs)
parseTypeSigniture ("fn":xs)  = 
  parseTypeSigniture xs >>= \(a,b) ->
  parseTypeSigniture b  >>= \(c,d) ->
  pure (Fn a c, d)
parseTypeSigniture x = Left $ Custom ("Unknown type: " <> show x) 

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
        in G . concatMap (replicate x) $ y
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
      Just [a, b, c] -> parseGif header a b c xs -- $ map (`addMarkBlock` new_name header) xs
      Just _         -> Left $ Custom "incorrect art header" (block_mark header)
      Nothing        -> parseScript header table (concatMap (words . unwrap) all) 
  in fun >>= \fn -> pure Data {
    currentName   = new_name header,
    typeSigniture = typeSig header,
    currentArgs   = [],
    function      = fn
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
  else traverse validateStrings (chunksOf h lns) >>= \gif -> 
    if length (concat gif) == f
      then pure $ \_ -> G $ 
            map (liftA2 (flip (,))  [h, h -1 .. 1] [1..w] `zip`) (concat gif)
      else Left $ Value "script's number of frames" "frames" f (length gif) (block_mark header) 
  where 
    validateStrings :: [Marked String] -> OrError [[Colored Char]]
    validateStrings [] = pure []
    validateStrings (x:xs) = firstStrLen >>= -- \lnlen -> 
      -- map concat . chunksOf h . map parseColorLine . rearange <$> helper (x:xs) lnlen
      fmap (map concat . chunksOf h . map parseColorLine . rearange) . helper (x:xs)
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

argTypes :: Type -> [Type]
argTypes (Fn a b) = a : argTypes b
argTypes _        = []

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
      else helper . concatMap (\(Marked m x) -> map (Marked m) (words x)) $ allah
        where
          helper :: [Marked String] -> [Marked String]
          helper = filter (\(Marked m x) -> (x `notElem` map fst builtinFns) && not (all (`elem` ('$':nums)) x))

parseChunks :: [Marked String] -> OrError (Map Name (NewHeader, [Marked String]))
parseChunks [] = pure []
parseChunks all@(Marked m x : xs) = 
  parse_header (Marked m x)  >>= \header -> 
  find_leftover xs           >>= \(inside, other) -> 
  ((new_name header, (header, map (`addMarkBlock` new_name header) inside)) :) <$> parseChunks other
  where 
    find_leftover :: [Marked String] -> OrError ([Marked String],[Marked String])
    find_leftover [] = Left $ Parse "code block" "end" "EOF" m
    find_leftover (Marked m a : as) = case trim a of
      "end" -> pure ([],as)
      _     -> first (Marked m a :) <$> find_leftover as

addMarkBlock :: Marked a -> Name -> Marked a
addMarkBlock (Marked m s) name = Marked m {block = pure name} s

parse_header :: Marked String -> OrError NewHeader
parse_header (Marked m s) = case words s of 
  (name:":":t) -> (m ?>) $ 
    parseName name  >>= \name -> 
    parseType t     >>= \sig -> 
    pure NewHeader {
      new_name   = name,
      typeSig    = sig,
      block_mark = m
    }
  _ -> Left $ Custom "A header has the syntax `name : type`" m

(?>) :: Mark -> OrToError a -> OrError a
(?>) m (Left f)  = Left (f m)
(?>) _ (Right x) = Right x

nums = ['0'..'9']

surround :: String -> String
surround x = case words x of
  [_] -> x
  _   -> "(" <> x <> ")"

isPossible :: Type -> Type -> Bool
isPossible w f@(Fn a b) = w == f || isPossible w b
isPossible w got        = w == got
  
new_evaluate :: Data -> OrToError ReturnType
new_evaluate Data {typeSigniture = Fn _ _} = Left $ Custom "can't new_evaluate a function"
new_evaluate Data {currentArgs = args, function = f} = pure $ f args

evaluateRealTypes :: Type -> [DummyData] -> OrToError DummyData
evaluateRealTypes want x = case evaluateTypes want x of
  RealError x -> Left x
  Success (x,[]) -> Right x
  Func f -> Left $ Custom "TypeMismatch in the first argument"

data ComplexError = RealError (Mark -> Error) | Func (DummyData -> Mark -> Error) | Success (DummyData, [DummyData]) 

evaluateTypes :: Type -> [DummyData] -> ComplexError
evaluateTypes want [] = Func (\name -> Custom (show name <> " is missing arguments"))
evaluateTypes (Type a) (x@Dummy {type_sig = Type b} : xs) = 
  if a == b then Success (x,xs) else Func (\name -> TypeMismatch name x)  
evaluateTypes want (x@Dummy {type_sig = tp} : xs) = if
  | tp == want -> Success (x,xs)
  | not (isPossible want tp) -> Func (\name -> TypeMismatch name x)
  | otherwise  -> case tp of
    Type a -> error (show want) --Func (flip TypeMismatch want) -- pure (x,xs)
    Fn a b -> case evaluateTypes a xs of
      RealError x             -> RealError x
      Func f                  -> RealError (f $ x)
      Success (arg, leftover) -> 
        case applyDummy x arg of
          Left x -> error "evaluateTypes "
          Right d -> case evaluateTypes want (d:leftover) of 
            RealError x             -> RealError x
            Func f                  -> RealError (f $ d)
            Success (arg, leftover) -> Success (arg, leftover)

applyDummy :: DummyData -> DummyData -> OrError DummyData
applyDummy Dummy {type_sig = Type _} _ = Left $ Custom "applied value to non-function" None
applyDummy 
  Dummy {type_sig = Fn a b, current_name = name1} 
  Dummy {type_sig = x, current_name = name2} =
    if x /= a 
    then Left $ Custom "applied value to non-function" None
    else pure Dummy {
      current_name = name1 <> " " <>surround name2,
      type_sig = b
    }

unsafe_eval :: Type -> [Data] -> Data
unsafe_eval = fst ... dangerous_evaluateExpression
  where
    dangerous_evaluateExpression :: Type -> [Data] -> (Data, [Data])
    dangerous_evaluateExpression (Type a) (x@Data {typeSigniture = Type _} :xs) = (x,xs)  
    dangerous_evaluateExpression want (x@Data {typeSigniture = Fn a b} : xs) = 
      if Fn a b == want then (x,xs) else
        let (arg, leftover) = dangerous_evaluateExpression a xs in
        dangerous_evaluateExpression want (dangerous_applyFn x arg : leftover) 


dangerous_applyFn :: Data -> Data -> Data
dangerous_applyFn 
  Data {typeSigniture = Fn a b, currentArgs = args, function = f, currentName = name} 
  arg@Data {currentName = name2} = 
  Data {
    currentName    =  name <> " " <> 
      if length (words name2) > 1 
      then "(" <> name2 <> ")" 
      else name2,
    typeSigniture  = b,
    currentArgs    = args <> [arg],
    function       = f
  } 
  
parseScript :: NewHeader -> Map Name Data -> [String] -> OrError ([Data] -> ReturnType)
parseScript NewHeader {typeSig = tp, block_mark = m} table xs = 
  -- TODO test if argument types match up
  helperParseScript xs >>= \newTable -> 
  first ($ m) (evaluateRealTypes (result tp) (map getDummy newTable) :: OrToError DummyData) >>
  pure (\args -> 
      let
        matcher x = case x of
          Left (i,_)  -> args !! (i - 1)
          Right x -> x
      in
      fromRight . evaluate . unsafe_eval (result tp) $ map matcher newTable 
    )
    where
      arguments = argTypes tp
      
      helperParseScript :: [String] -> OrError [Either (Int, Type) Data]
      helperParseScript [] = pure []
      helperParseScript (('$':num):xs) = case readMaybe num of
        Nothing -> Left $ Custom "$ must be preceded by a number" m
        Just x  -> case arguments !? x  of
          Nothing -> 
            Left $ Custom "index is greater than the number of arguments" m 
          Just t ->
            (Left (x,t) :) <$> helperParseScript xs
      helperParseScript (x:xs) = 
        case readMaybe x of  -- all (`elem` nums) x 
          Just num ->             
            let 
              d = Data {
                currentName   = x,
                typeSigniture = Type Int,
                currentArgs   = [],
                function      = const (I num)
              }
            in (Right d :) <$> helperParseScript xs
          Nothing -> case lookup x table of
            Nothing -> Left $ Custom "variable not in scope" m
            Just x  -> (Right x :) <$> helperParseScript xs

      getDummy :: Either (Int, Type) Data -> DummyData
      getDummy (Left (i, t)) = Dummy {
        current_name = '$' : show i,
        type_sig     = t
      }
      getDummy (Right Data {currentName = name, typeSigniture = t}) = Dummy {
        current_name = name,
        type_sig     = t
      }

(!?) :: [a] -> Int -> Maybe a
(!?) y x = if x > length y 
           then Nothing 
           else pure $ y !! (x - 1)
