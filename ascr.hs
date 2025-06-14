{-# Language MultiWayIf, LambdaCase #-}

module Main (main) where

import System.Exit (die)
import System.Process (callCommand)
import System.Environment (getArgs)
import Control.Applicative (liftA2)
import Control.Monad ((<=<), (>=>), guard, unless, when)
import Control.Arrow ((<<<), (>>>), (***), (&&&), (+++), (|||))
import qualified Data.Map.Strict as Map 
import Data.Bifunctor (first, second, bimap)
import Data.Either (fromRight, fromLeft)
import Data.Maybe (fromJust, listToMaybe, isNothing, fromMaybe)
import Data.Tuple (swap)
import Data.List (nub, transpose, sortOn, intercalate)
import Text.Read (readMaybe) 

infix 8 ...

type ShellScript   = String
type Map a b       = [(a,b)]
type OrError       = Either Error
type LineNumber    = Int
type Dependencies  = Map Name [Name]
type Name          = String
type Frame         = [Colored Char]
type Gif           = [Frame]
type Coordinate    = (Int,Int)
type RealGif = [Map Coordinate Character]

data ValName = Name String [ValName]

data Type = Type SimpleType | Fn Type Type deriving Eq

data Block = Block NewHeader [Marked String] BlockType

data BlockType =  Art Int Int | Script

data NewHeader = NewHeader {
  new_name    :: String,
  typeSig     :: Type,
  block_mark  :: Mark
} deriving Show

instance Show Type where
  show (Type x) = colour Blue (show x)
  show (Fn a b) = colour Blue "fn " <> show a <> " " <> show b

data SimpleType = Int | Giff | Colour deriving Eq

instance Show SimpleType where
  show Int    = "int"
  show Giff   = "gif"
  show Colour = "color"

data Data = Data {
  dummy         :: DummyData,
  currentArgs   :: [Data],
  function      :: [Data] -> ReturnType
}

data DummyData = Dummy {
  current_name   :: String,
  type_sig       :: Type
}

instance Show DummyData where
  show Dummy {current_name = name, type_sig = tp} = colour Magenta name <> " : " <> show tp

data ReturnType = I Int | G RealGif | C Color deriving Show

data OutputFile = Gif | Image deriving Eq

newtype Suggestion = Suggestion (Maybe String) 

instance Show Suggestion where
  show (Suggestion Nothing) = ""
  show (Suggestion (Just x)) = ". Did you mean " <> colour Green x <> "?"

data Mark 
  = Arguments
  | None
  | File {
      origin :: FilePath, 
      line   :: LineNumber, 
      block  :: Maybe Name
    }

data Marked a = Marked Mark a deriving Show

instance Functor Marked where
  fmap f (Marked a b) = Marked a (f b)

getArg (Fn a _) = a

data Error = Error {
  errorType :: ErrorType,
  errorMark :: Mark
}

instance Show Error where
  show Error {errorType = t, errorMark = m} = show m <> show t

data ErrorType =
  -- = Delimiter String
    BadDelimiter String
  | Parse String String String
  | Value String String Int Int
  | Custom String
  | NoMatchingName Name Suggestion 
  | Recursive Name [Name]
  | ArgError String
  | Help
  | ReallyCustom String
  | TypeMismatch DummyData DummyData
  | EmptyGif
  | MissingBody

instance Show ErrorType where 
  show err = case err of
    Help ->
      "Usage: ascr [\x1b[33mOPTIONS\x1b[0m] \x1b[35mFILE\x1b[0m\n\nOptions:\n"
      <> "  \x1b[33m-c\x1b[0m        Don't write to a file\n"
      <> "  \x1b[33m-d\x1b[0m \x1b[36mDIR\x1b[0m    Directory in which to save the gif\n"
      <> "  \x1b[33m-f\x1b[0m \x1b[32mNUM\x1b[0m    Frames per second\n"
      <> "  \x1b[33m-h\x1b[0m        Show this help text\n"
      <> "  \x1b[33m-m\x1b[0m        Paste StdIn as a comment into the output script\n"
      <> "  \x1b[33m-n\x1b[0m \x1b[35mNAME\x1b[0m   which gif to evaluate\n"
      <> "  \x1b[33m-q\x1b[0m        Suppress success gif\n"
      <> "  \x1b[33m-t\x1b[0m a      Where a is either `vid` or `gif`"

    TypeMismatch a b -> format "% expected a value of type %.\nHowever, % could never evaluate to it"
      [show a, (show . getArg . type_sig) a, show b]

    BadDelimiter s -> format "Unexpected closing deliminator `%` found"
      [colour Red s] 

    Parse thing expected got -> format "Couldn't parse %. Expected % but got `%`"
      [thing, expected, colour Red got]

    Value thing name expected got -> format "Couldn't parse %. Expected % % but got %"
      [thing, show expected, name, colour Red (show got)]

    Custom s -> s 

    NoMatchingName a suggestion -> format "Could not find the gif `%` in the input files%"
      [colour Magenta a, show suggestion]

    Recursive a l -> format "The script % called itself recursively.\n"
      [colour Magenta a, foldr1 (\x y -> colour Magenta x <> " -> " <> colour Magenta y) l]

    ArgError s -> s <> ". Check " <> colour Yellow "ascr -h"

    ReallyCustom x -> x

    EmptyGif -> "The gif is empty"

    MissingBody -> "The block is missing a body"

instance Show Mark where
  show x = "\x1b[31;1mError\x1b[0m" <> (
    case x of
      File {origin = o, line = l, block = b} ->
        format
        " at line % in %%"
        [ show (Colored Cyan l)
        , colour Cyan o
        , maybe "" (\name -> format " (in the definition of %)" [colour Magenta name]) b
        ]
      Arguments -> " in the \x1b[33marguments\x1b[0m"
      None      -> ""
    ) <> ":\n"
    
format :: String -> [String] -> String
format [] _ = []
format ('%':xs) [] = error xs
format ('%':xs) (a:as) = a <> format xs as
format (x:xs) as = x : format xs as

data Modifiers = Modifiers {
  target      :: String,
  frameTime   :: Float,
  directory   :: FilePath,
  message     :: Bool,
  quiet       :: Bool,
  check       :: Bool,
  text        :: Bool,
  output      :: OutputType
} deriving Show

data OutputType = Single | Looping deriving Show

data Colored a = Colored Color a deriving Eq

data Character = Space | Character Char Color deriving Eq

instance Show Character where
  show Space = " "
  show (Character char color) = colour color (show char)

data Color  
  = Black    
  | Red      
  | Green    
  | Yellow   
  | Blue     
  | Magenta  
  | Cyan
  | White
  deriving (Show, Eq)

instance Show a => Show (Colored a) where
  show (Colored color c) = colour color (show c)

colour :: Color -> String -> String 
colour c s = colorCode c <> s <> "\x1b[0m"

colorCode :: Color -> String
colorCode Black   = "\x1b[30m"
colorCode Red     = "\x1b[31m"
colorCode Green   = "\x1b[32m"
colorCode Yellow  = "\x1b[33m"
colorCode Blue    = "\x1b[34m"
colorCode Magenta = "\x1b[35m"
colorCode Cyan    = "\x1b[36m"
colorCode White   = "\x1b[37m"


main = getArgs >>= \arg -> case parseArgs arg defaultMods of
  Left e -> exitWithError e
  Right (mods@Modifiers {
    output = o, target = t, directory = d,
    quiet  = q, check  = c, message   = m
  }, files) ->
    (
      if m
      then pure <$> (putStrLn "enter a message:" >> getContents)
      else pure Nothing
    ) >>= \messageIn ->
    mapM readFile files >>= \fl ->
    case (newGiga (Marked Arguments t) mods . concat . zipWith number files) fl >>= evaluate of
      Left e -> exitWithError e
      Right (I int) -> print int
      Right (G gif) -> case changeFormat gif of
        Left e -> exitWithError Error {errorType = e, errorMark = None}
        Right (width, height, newGif) ->
          let
            file = d <> "/" <> t <> ".sh"
            (fileSh, command) = formatShell mods width height messageIn . pipeline $ newGif
          in
          putStr "\x1b[32;1mSuccess!\x1b[0m\n" >>
          (
            if width > 80
            then putStrLn "\x1b[33;1mWarning:\x1b[0m video should not be over 80 chars in width"
            else if height > 24
              then putStrLn "\x1b[33;1mWarning:\x1b[0m video should not be over 24 chars in height"
              else pure ()
          ) >>
          unless c (
            writeFile file (case o of
              Looping -> fileSh
              Single  -> command
            ) >> 
            callCommand ("chmod +x " <> file) >>
            putStrLn ("Gif saved to " <> colour Cyan file <> ".")
          ) >> 
          unless q (callCommand command)-- (callCommand command)

formatShell :: Modifiers -> Int -> Int -> Maybe String -> [Either String Int] -> (ShellScript, ShellScript)
formatShell mods wd ht message renderedFrames = case renderedFrames of
  [Left frame] -> (init2 <> gif <> "\\n'", init2 <> hideprompt <> initMove <> cleanup <> gif <> "'\nsleep 2" <> "\ncleanup")
    where gif = "printf '" <> frame
  frames  -> (init2 <> hideprompt <> initMove <> cleanup <> intro <> alloc <> loop <> body <> done, init2 <> initMove <> cleanup <> intro <> alloc <> body <> "\nprintf \x1b[" <> show (ht - 1) <> "A\r\x1b[0J\x1b[1m")
    where 
      newHelper :: [Either String Int] -> String
      newHelper [] = ""
      newHelper (Left s : xs) = "    draw '" <> s <> "'\n" <> newHelper xs
      newHelper (Right i : xs) = "    sleep " <> show (fromIntegral i * frameTime mods) <> "\n" <> newHelper xs
      intro = "draw() {\n    printf \"$move_up$1\"\n    sleep " <> 
        show (frameTime mods) <> "\n}\n\n"
      alloc = "yes '' | head -n " <> show (ht - 1) <> "\n\n"
      loop = "while true\ndo\n"
      body = newHelper frames
      -- body = concat (helper ({-map (init . init)-} frames) "" 0.0)
      done = "done"
  where
    initialize = "VIDEO_WIDTH=" <> show wd <> "\nVIDEO_HEIGHT=" <> show ht <> "\n\n"
    hideprompt = "stty -echo\nprintf '\\033[?25l'\n\n"
    initMove = "move_up=\"\\033[" <> show (ht - 1) <> "F\"\n\n"
    cleanup = "cleanup() {\n    printf \"$move_up\\033[0J\\033[0m\\033[?25h\"\n    stty echo\n    exit 0\n}\n\ntrap cleanup INT\n\n"
    comment = "#!/bin/sh\n" <> maybe "" (('\n' :) . unlines . map ("# " <>) . lines) message <> "\n"
    sizeCheck = "if [ $(tput cols) -lt " <> show wd <> " -o $(tput lines) -lt " <> show ht <> " ]\nthen\n    printf \"\\033[91mterminal is too small\\nmust be at least " <> show wd <> " by " <> show ht <> " cells\\033[0m\\n\" >&2\n    exit 1\nfi\n\n" 
    clear = "\nprintf \"$move_up\\033[0J\\033[?25h\\033[0m\"\n"
    init2 = comment <> sizeCheck

dimensions :: RealGif -> Either ErrorType (Int, Int, Int, Int)
dimensions gif = case concatMap (map fst) gif of
  [] -> Left EmptyGif
  g  -> pure $ getDimensions g
    where
      getDimensions :: [Coordinate] -> (Int, Int, Int, Int)
      getDimensions x =
        let
          xCoords = map fst x
          yCoords = map snd x
        in
          (
            minimum xCoords,
            maximum xCoords,
            minimum yCoords,
            maximum yCoords
          )

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
  target    = "main",
  output    = Looping
}

-- TODO
-- check that it's not 0
changeFormat :: RealGif -> Either ErrorType (Int, Int, [[[Character]]])
changeFormat x = dimensions x >>= \(x_min, x_max, y_min, y_max) ->
  let
    chart = liftA2 (flip (,))  [y_max, y_max -1 .. y_min] [x_min..x_max]
    convertFrame :: Map Coordinate Character -> [[Character]]
    convertFrame a =  chunksOf (x_max - x_min + 1) $ map lookupChar chart
      where
        lookupChar want = fromMaybe Space $ lookup want a
  in
  pure (x_max - x_min + 1, y_max - y_min + 1, map convertFrame x) 

parseInt :: String -> String -> Either ErrorType Int
parseInt x y = case readMaybe x of
  Just x  -> pure x
  Nothing -> Left $ Parse y "an int" x

parseName :: String -> Either ErrorType Name
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
  "-h"        -> Left Error {errorType = Help, errorMark = None}
  "-n"        -> getNext a as >>= \(b,cs) -> parseArgs cs     m {target    = b   }
  "-c"        -> parseArgs as m {check     = True}
  "-q"        -> parseArgs as m {quiet     = True}
  "-m"        -> parseArgs as m {message   = True}  
  "-d"        -> getNext a as >>= \(b,cs) -> parseArgs cs     m {directory = b   }  
  -- "-s"        -> parseArgs as m {text      = True}
  "-t"        -> getNext a as >>= \(b,cs) -> case b of
    "vid" -> parseArgs cs m {output = Single}
    "gif" -> parseArgs cs m {output = Looping}
    other -> Left Error {
      errorType = ArgError ("The \x1b[33m-t\x1b[0m flag expected either `gif` or `vid`. Got" <> other),
      errorMark = Arguments
    }
  "-f"        -> getNext a as >>= \(b,cs) -> case readMaybe b >>= maybeGuard (>= 0) of 
    Nothing -> Left Error {
      errorType = ArgError "The \x1b[33m-f\x1b[0m flag expected a positive \x1b[33mNUM\x1b[0m",
      errorMark = Arguments
    }
    Just x  -> parseArgs cs m {frameTime = 1 / x}
  ('-':'-':x) -> Left Error {
    errorType = ArgError $ "Unknown argument '" <> colour Yellow ('-':'-':x) <> "'",
    errorMark = Arguments
  }
  ['-',x]     -> Left Error {
    errorType = ArgError $ "Unknown argument '" <> colour Yellow ['-',x] <> "'",
    errorMark = Arguments
  }
  ('-':x)     -> parseArgs (map (cons '-' . pure) x <> as) m
  _           -> pure (m, a:as)
  where 
    getNext :: String -> [String] -> OrError (String, [String])
    getNext s []   = Left Error {
      errorType = Custom (s <> " expected an argument"),
      errorMark = Arguments
    }
    getNext _ [_]  = Left Error {
      errorType = Custom "Expected a list of files at the end",
      errorMark = Arguments
    }
    getNext _ (x:xs)    = pure (x, xs)
parseArgs _ _ = Left Error {
  errorType = ArgError "Args should end with \x1b[33mNAME <FILE>\x1b[0m",
  errorMark = Arguments
}

cons = (:)

append :: a -> [a] -> [a]
append  x y = y <> [x]

win :: Map Name Data -> Dependencies -> Map Name Block -> OrError (Map Name Data)
win uwu []          _ = pure uwu
win uwu ((n,[]):xs) d = 
  -- let (header@NewHeader {new_name = name}, str) = fromJust $ lookup n d in
  parseBlock (fromJust $ lookup n d) uwu >>= \s -> 
  win ((n,s):uwu) (second (filter (/= n)) <$> xs) d
win uwu (x:xs)      d = win uwu (append x xs) d

unwrap :: Marked a -> a
unwrap (Marked _ a) = a  
    
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

rotate [] = []
rotate (x:xs) = xs <> [x]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n x  = uncurry ((. chunksOf n) . cons) $ splitAt n x

removeExtraSpaces :: [Character] -> [Character]
removeExtraSpaces = reverse . remove . reverse
  where
    remove (Space : as) = remove as
    remove as = as

clean :: Char -> String
clean '\\' = "\\134"
clean '\'' = "\\047"
clean '\n' = "\\n"
clean '%'  = "%%"
clean a    = pure a

pipeline :: [[[Character]]] -> [Either String Int]
pipeline (x:xs) = case reduce2 $ (x:xs) of
  [x] -> pure . Left . fromJust . formatCommands . intercalate [Move Down] . map (map Draw) $ map removeExtraSpaces x
  xs -> (Left (init . init $ renderer x) :) . init . countUp . map formatCommands . reduce $ xs
    where
      countUp :: [Maybe String] -> [Either String Int]
      countUp [] = []
      countUp (Just s: xs) = Left s : countUp xs
      countUp (Nothing: xs) = let (num, other) = first ((+ 1) . length) $ span isNothing xs in Right num : countUp other
      
data Command = Draw Character | Move Dir
data Dir = Down | Next

reduce :: [[[Character]]] -> [[Command]]
reduce [] = []
reduce [x] = pure . intercalate [Move Down] . map (map Draw) $ map removeExtraSpaces x
reduce (x:xs) = reverse . map (uncurry (helper Black)) $ chunksOf2 (x: reverse (x: xs))
  where 
    helper :: Color -> [[Character]] -> [[Character]] -> [Command]
    helper _ [] [] = []
    helper _ ([[]]) ([[]]) = []
    helper c ([]:xs) ([]:ys) = Move Down : helper c xs ys
    helper c ((x:xx):xs) ((y:yy):ys) = 
        if x == y 
        then 
          let 
            (same, different) = first (map fst) . span (uncurry (==)) $ zip (x:xx) (y:yy) 
            len = length same
            leftover = uncurry (helper c) . bimap (: xs) (: ys) $ unzip different
          in 
          if different /= [] && sum (map (getSize c) same) < 7 
          then map Draw same <> leftover
          else take len (repeat $ Move Next) <> leftover
        else Draw x : helper (case getColor x of Nothing -> c; Just x -> x) (xx: xs) (yy: ys)

getSize :: Color -> Character -> Int
getSize c = \case
    Space -> 1
    Character char color -> (if color == c then 0 else 7) + case char of
        '%'  -> 2
        '\'' -> 4
        '\\' -> 4
        _ -> 1

getColor = \case
  Space -> Nothing
  Character _ color -> Just color

formatCommands :: [Command] -> Maybe String
formatCommands x = if all isMove x then Nothing else pure $ showCommands x Black
  where
    isMove (Move _) = True
    isMove _ = False

showCommands :: [Command] -> Color -> String
showCommands [] _ = []
showCommands m@(Move x : xs) oldColor = case cleanMove m of 
  (f, []) -> show f {next = 0}
  (f, other) -> show f <> showCommands other oldColor
  -- where (str, other) = cleanMove m
showCommands (Draw x : xs) oldColor = case x of
  Space -> ' ' : showCommands xs oldColor
  c@(Character char color) -> 
    if color == oldColor 
    then clean char <> showCommands xs oldColor 
    else colorChar c <> showCommands xs color 

data FinalMovement = Final {next :: Int, down :: Int} 

instance Show FinalMovement where
  show Final {next = next, down = down} = y <> x
    where 
      y = case down of 
        0 -> "" 
        1 -> "\\n"
        2 -> "\\n\\n"
        3 -> "\\n\\n\\n"
        _ -> "\\033[" <> show down <> "E" 
      x = if next == 0 then "" else "\\033[" <> show next <> "C"

cleanMove :: [Command] -> (FinalMovement, [Command])
cleanMove x = first (flip makeFinal Final {next = 0, down = 0}) $ helper x
  where 
    helper :: [Command] -> ([Dir], [Command])
    helper (Move x: xs) = first (x:) $ helper xs
    helper x = ([], x)

    makeFinal :: [Dir] -> FinalMovement -> FinalMovement
    makeFinal [] f = f
    makeFinal (Next : xs) f = makeFinal xs f {next = next f + 1, down = down f} 
    makeFinal (Down : xs) f = makeFinal xs f {next = 0, down = down f + 1} 
    
renderer :: [[Character]] -> String
renderer = helper Black . concatMap (append (Character '\n' Black))
  where
    helper :: Color -> [Character] -> String
    helper _        [] = "" --"\\n"
    helper oldcolor (Space:xs) = ' ' : helper oldcolor xs
    helper oldcolor (c@(Character char color) : xs) =
      if color == oldcolor || char == '\n'
      then clean char <> helper oldcolor xs
      else  colorChar c <> helper color xs

chunksOf2 :: [a] -> [(a, a)] 
chunksOf2 [] = []
chunksOf2 [a] = []
chunksOf2 (a:b:bs) = (a, b) : chunksOf2 (b:bs)

reduce1 :: [[[Character]]] -> [[[Character]]]
reduce1 = map (map removeExtraSpaces)

reduce2 :: [[[Character]]] -> [[[Character]]]
reduce2 [] = []
reduce2 (x:xs) = if all (== x) xs then [x] else x:xs

parseColorLine :: String -> [(Maybe Color, Char)]
parseColorLine x = uncurry zip . first (map charToColor) . swap $ splitAt (length x `quot` 2) x
  where 
    charToColor = \case 
      '.' -> Nothing
      '0' -> Just Black 
      '1' -> Just Red
      '2' -> Just Green
      '3' -> Just Yellow
      '4' -> Just Blue
      '5' -> Just Magenta
      '6' -> Just Cyan
      '7' -> Just White
      _   -> Just White

colorChar :: Character -> String
colorChar Space = " "
colorChar (Character s c) = "\\033[9" <> case c of
    Black   -> "0"
    Red     -> "1"
    Green   -> "2"
    Yellow  -> "3"
    Blue    -> "4"
    Magenta -> "5"
    Cyan    -> "6"
    White   -> "7"
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
evaluate Data {dummy = Dummy {type_sig = Fn _ _}} = Left Error {
  errorType = Custom "can't evaluate a function",
  errorMark = None
}
evaluate Data {currentArgs = args, function = f} = pure $ f args

parseType :: [String] -> Either ErrorType Type
parseType x = parseTypeSigniture x >>= \case
  (t,[]) -> pure t
  _      -> Left $ Custom "Trailing words after the type signiture"
parseTypeSigniture :: [String] -> Either ErrorType (Type, [String])
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
  "<o>" -> Left Error {
    errorType = BadDelimiter "<o>",
    errorMark = m
  }
  _     -> (Marked m (stripWhitespace s) :) <$> cleanInput xs
  where
    findClosing [] = pure []
    findClosing (Marked m a : as) = case trim a of
      "<o>" -> pure as
      "---" -> Left Error {
        errorType = BadDelimiter "---",
        errorMark = m
      }
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
    ("anchor"      , anchorf      ),
    ("seq"         , seqf         ),
    ("take"        , takef        ),
    ("tail"        , tailf        ),
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
    tailf = Data {
      dummy = Dummy {
        current_name = "tail",
        type_sig     = Fn (Type Giff) (Type Giff)
      },
      currentArgs = [],
      function = \[a] ->
        let
          Right (G x) = evaluate a
        in
        G (tail x)

    }
    movef = Data {
      dummy = Dummy {
        current_name   = "move",
        type_sig = Fn (Type Int) (Fn (Type Int) (Fn (Type Giff) (Type Giff)))
      },
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
      dummy = Dummy {
        current_name   = "reverse",
        type_sig = Fn (Type Giff) (Type Giff)
      },
      currentArgs   = [],
      function      = \[a] ->
        let Right (G x) = evaluate a in
        G $ reverse x
    }

    skipf = Data {
      dummy = Dummy {
        current_name   = "skip",
        type_sig = Fn (Type Int) (Fn (Type Giff) (Type Giff))
      },
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
      dummy = Dummy {
        current_name   = "join",
        type_sig = Fn (Type Giff) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[a,b] ->
        let
          Right (G x) = evaluate a
          Right (G y) = evaluate b

          frames = length x `lcm` length y in 
        G . map (uncurry mappend) . take frames $ zip (cycle x) (cycle y)
    }

    nullf = Data {
      dummy = Dummy {
        current_name   = "null",
        type_sig = Type Giff
      },
      currentArgs   = [],
      function      = \_ ->
        G [[]]
    }

    anchorf = Data {
      dummy = Dummy {
        current_name   = "anchor",
        type_sig = Type Giff
      },
      currentArgs   = [],
      function      = \_ ->
        G [[((0, 0), Space)]]
    }

    slowf = Data {
      dummy = Dummy {
        current_name   = "slow",
        type_sig = Fn (Type Int) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[a,b] ->
        let 
          Right (I x) = evaluate a
          Right (G y) = evaluate b
        in G . concatMap (replicate x) $ y
    } 

    seqf = Data {
      dummy = Dummy {
        current_name   = "seq",
        type_sig = Fn (Type Giff) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[a,b] ->
        let
          Right (G x) = evaluate a
          Right (G y) = evaluate b
        in 
        G $ x <> y
    }

    takef = Data {
      dummy = Dummy {
        current_name   = "take",
        type_sig = Fn (Type Int) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[a,b] ->
        let 
          Right (I x) = evaluate a
          Right (G y) = evaluate b
        in 
        G . take x $ cycle y
    }

    frame_countf = Data {
      dummy = Dummy {
        current_name   = "frame_count",
        type_sig = Fn (Type Giff) (Type Int)
      },
      currentArgs   = [],
      function      = \[a] ->
        let
          Right (G x) = evaluate a
        in I $ length x
    }

    dyef = Data {
      dummy = Dummy {
        current_name   = "dye",
        type_sig = Fn (Type Colour) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[a,b] ->
        let 
          Right (C x) = evaluate a
          Right (G y) = evaluate b
          recolor Space = Space
          recolor (Character s _) = Character s x 
        in G $ map (map (second recolor)) y
    }

    createColor :: Color -> Data
    createColor x = Data {
      dummy = Dummy {
        current_name = show x,
        type_sig     = Type Colour
      },
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

-- parseBlock :: NewHeader -> [Marked String] -> Map Name Data -> OrError Data
parseBlock :: Block -> Map Name Data -> OrError Data
parseBlock (Block header lns tp) table =
  let
    output = case tp of
      Art x y ->
        parseGif header x y lns
      Script ->
        parseScript header table (concatMap (words . unwrap) lns) 
  in
  output >>= \fn -> pure Data {
      dummy = Dummy {
        current_name = new_name header,
        type_sig     = typeSig header
      },
      currentArgs   = [],
      function      = fn
    }

parseGif :: NewHeader -> Int -> Int -> [Marked String] -> OrError ([Data] -> ReturnType)
parseGif header w h lns = 
  if mod (length lns) h /= 0 
  then Left Error {
    errorType = Custom $
      "A gif's number of lines should be divisible by the header's height.\nYou have "
      <> show (length lns) <> " lines.",
    errorMark = block_mark header
  }
  else traverse validateStrings (chunksOf h lns) >>= \gif -> 
      pure $ \_ ->
      G . map (removeTransp . zip (liftA2 (flip (,)) [h -1 , h -2 .. 0] [0 .. w - 1])) $ concat gif
  where 
    removeTransp :: Map Coordinate (Maybe Color, Char) -> Map Coordinate Character
    removeTransp [] = []
    removeTransp ((_, (Nothing,_)):xs) = removeTransp xs
    removeTransp ((coord, (Just color, ' ' )):xs) = (coord, Space) : removeTransp xs
    removeTransp ((coord, (Just color, char)):xs) = (coord, Character char color) : removeTransp xs
        
    validateStrings :: [Marked String] -> OrError [[(Maybe Color, Char)]]
    validateStrings [] = pure []
    validateStrings (x:xs) = firstStrLen >>= -- \lnlen -> 
      -- map concat . chunksOf h . map parseColorLine . rearange <$> helper (x:xs) lnlen
      fmap (map concat . chunksOf h . map parseColorLine . rearange) . helper (x:xs)
      where
        firstStrLen = let lnlen = length . stripWhitespace $ unwrap x in
          if lnlen `mod` w == 0
          then pure lnlen
          else Left Error {
            errorType = Custom $
              "A gif's chars per line should be divisible by the header's width.\nYou have "
              <> show lnlen <> " chars.",
            errorMark = getMark x
          }

        rearange :: [String] -> [String]
        rearange = concat . transpose . map (chunksOf (w * 2))

        helper :: [Marked String] -> Int -> OrError [String]
        helper [] _ = pure []
        helper ((Marked m x):xs) n = let len = length x in
          if len /= n
          then Left Error {
            errorType = Value "line" "characters" n len,
            errorMark = m
          }
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

findDependencies :: Map Name Block -> Marked Name -> OrError (Map Name [Name])
findDependencies table = fmap nub . getDependencies []
  where 
    getDependencies :: [Name] -> Marked Name -> OrError (Map Name [Name])
    getDependencies used (Marked m target) = if
      | elem target (map fst builtinFns) -> pure []
      | elem target used -> Left Error {
          errorType = Recursive target (reverse $ target : used),
          errorMark = None
        }
      | otherwise -> case extract <$> lookup target table :: Maybe [Marked String] of 
        Nothing -> Left Error {
          errorType = NoMatchingName target (findSimilarName target (map fst table)),
          errorMark = m
        }
        Just x  -> cons (target, map unwrap x) . concat <$> traverse (getDependencies (target:used)) x
        
    extract :: Block -> [Marked Name]
    extract (Block _ _ (Art _ _)) = []
    extract (Block header lns Script) = 
      helper . concatMap (\(Marked m x) -> map (Marked m) (words x)) $ lns
        where
          helper :: [Marked String] -> [Marked String]
          helper = filter (\(Marked m x) -> (x `notElem` map fst builtinFns) && not (all (`elem` ('$':nums)) x))

parseChunks :: [Marked String] -> OrError (Map Name Block)-- OrError (Map Name (NewHeader, [Marked String]))
parseChunks [] = pure []
parseChunks (Marked m ('-':'-':_) : xs) = parseChunks xs 
parseChunks all@(Marked m x : xs) = 
  parseHeader (Marked m x)  >>= \header -> 
  find_leftover xs           >>= \(inside, other) ->
    case inside  of
      [] -> Left Error {errorType = MissingBody, errorMark = block_mark header}
      all@(Marked m ln:lns) ->
        let
          (lines, tp) = case traverse readMaybe $ words ln of
            Just [a, b] -> (filter (not . isArtComment a . unwrap) lns, Art a b)
            Nothing     -> (filter (not . isComment . trim . unwrap) all,    Script)
        in
        ((new_name header, Block header lines tp):) <$> parseChunks other
  where 
    find_leftover :: [Marked String] -> OrError ([Marked String],[Marked String])
    find_leftover [] = Left Error {
      errorType = Parse "code block" "end" "EOF",
      errorMark = m
    }
    find_leftover (Marked m a : as) = case trim a of
      "end" -> pure ([],as)
      _     -> first (Marked m a :) <$> find_leftover as

isComment :: String -> Bool
isComment ('-':'-':_) = True
isComment _ = False

isArtComment :: Int -> String -> Bool
isArtComment width ('-':'-':' ':xs) = length xs + 3 `mod` width * 2 /= 0
isArtComment _ "--" = True
isArtComment _ _ = False

addMarkBlock :: Marked a -> Name -> Marked a
addMarkBlock (Marked m s) name = Marked m {block = pure name} s

mkError :: Mark -> Either ErrorType a -> Either Error a
mkError _ (Right x) = Right x
mkError m (Left e) = Left Error {
  errorType = e,
  errorMark = m
}

parseHeader :: Marked String -> OrError NewHeader
parseHeader (Marked m s) = case words s of 
  (name:":":t) -> mkError m $ 
    parseName name  >>= \name -> 
    parseType t     >>= \sig -> 
    pure NewHeader {
      new_name   = name,
      typeSig    = sig,
      block_mark = m
    }
  _ -> Left Error {
    errorType = Custom "A header has the syntax `name : type`",
    errorMark = m
    }

nums = '-' : ['0'..'9']

surround :: String -> String
surround x = case words x of
  [_] -> x
  _   -> "(" <> x <> ")"

isPossible :: Type -> Type -> Bool
isPossible w f@(Fn a b) = w == f || isPossible w b
isPossible w got        = w == got
  
evaluateRealTypes :: Type -> [DummyData] -> Either ErrorType DummyData
evaluateRealTypes want x = case evaluateTypes want x of
  RealError x    -> Left x
  Success (x,[]) -> Right x
  Success (x,xs) -> Left $ Custom (show x <> " did not expect any arguments")
  Func f         -> Left $ Custom "TypeMismatch in the first argument"

data ComplexError = RealError ErrorType | Func (DummyData -> ErrorType) | Success (DummyData, [DummyData]) 

evaluateTypes :: Type -> [DummyData] -> ComplexError
evaluateTypes want [] = Func (\name -> Custom (show name <> " is missing arguments"))
evaluateTypes (Type a) (x@Dummy {type_sig = Type b} : xs) = 
  if a == b then Success (x,xs) else Func $ flip TypeMismatch x  
evaluateTypes want (x@Dummy {type_sig = tp} : xs) = if
  | tp == want -> Success (x,xs)
  | not (isPossible want tp) -> Func $ flip TypeMismatch x
  | otherwise  -> case tp of
    Type a -> error (show want) --Func (flip TypeMismatch want) -- pure (x,xs)
    Fn a b -> case evaluateTypes a xs of
      RealError x             -> RealError x
      Func f                  -> RealError (f x)
      Success (arg, leftover) -> 
        case applyDummy x arg of
          Left x -> error "evaluateTypes "
          Right d -> case evaluateTypes want (d:leftover) of 
            RealError x             -> RealError x
            Func f                  -> RealError (f d)
            Success (arg, leftover) -> Success (arg, leftover)

applyDummy :: DummyData -> DummyData -> OrError DummyData
applyDummy Dummy {type_sig = Type _} _ = Left Error {
  errorType = Custom "applied value to non-function",
  errorMark = None
}
applyDummy 
  Dummy {type_sig = Fn a b, current_name = name1} 
  Dummy {type_sig = x, current_name = name2} =
    if x /= a 
    then Left Error {
      errorType = Custom "applied value to non-function",
      errorMark = None
    }
    else pure Dummy {
      current_name = name1 <> " " <> surround name2,
      type_sig = b
    }

unsafeEval :: Type -> [Data] -> Data
unsafeEval = fst ... unsafeEvaluateExpression
  where
    unsafeEvaluateExpression :: Type -> [Data] -> (Data, [Data])
    unsafeEvaluateExpression (Type a) (x@Data {dummy = Dummy {type_sig = Type _}} :xs) = (x,xs)  
    unsafeEvaluateExpression want (x@Data {dummy = Dummy {type_sig = Fn a b}} : xs) = 
      if Fn a b == want then (x,xs) else
        let (arg, leftover) = unsafeEvaluateExpression a xs in
        unsafeEvaluateExpression want (unsafeApplyFn x arg : leftover) 


unsafeApplyFn :: Data -> Data -> Data
unsafeApplyFn 
  Data {dummy = d@Dummy {type_sig = Fn a b}, currentArgs = args, function = f} 
  arg@Data {dummy = d2} = 
  Data {
    dummy = case applyDummy d d2 of
      Right x -> x
      Left _ -> error "can't happen",
    currentArgs    = args <> [arg],
    function       = f
  } 
  
parseScript :: NewHeader -> Map Name Data -> [String] -> OrError ([Data] -> ReturnType)
parseScript NewHeader {typeSig = tp, block_mark = m} table xs = 
  helperParseScript xs >>= \newTable -> 
  mkError m (evaluateRealTypes (result tp) (map getDummy newTable) :: Either ErrorType DummyData) >>
  pure (\args -> 
      let
        matcher x = case x of
          Left (i,_)  -> args !! (i - 1)
          Right x -> x
      in
      case evaluate . unsafeEval (result tp) $ map matcher newTable of
        Right x -> x
        Left x -> error "can't happen 2"
    )
    where
      arguments = argTypes tp
      
      helperParseScript :: [String] -> OrError [Either (Int, Type) Data]
      helperParseScript [] = pure []
      helperParseScript (('$':num):xs) = case readMaybe num of
        Nothing -> Left Error {
          errorType = Custom "$ must be preceded by a number",
          errorMark = m
        }
        Just x  -> case arguments ??? x  of
          Nothing -> 
            Left $ Error {
              errorType = Custom "index is greater than the number of arguments",
              errorMark = m
            }
          Just t ->
            (Left (x,t) :) <$> helperParseScript xs
      helperParseScript (x:xs) = 
        case readMaybe x of  -- all (`elem` nums) x 
          Just num ->             
            let 
              d = Data {
                dummy = Dummy {
                  current_name   = x,
                  type_sig = Type Int
                },
                currentArgs   = [],
                function      = const (I num)
              }
            in cons (Right d) <$> helperParseScript xs
          Nothing -> case lookup x table of
            Nothing -> Left $ Error {
              errorType = Custom "variable not in scope",
              errorMark = m
            }
            Just x  -> cons (Right x) <$> helperParseScript xs

      getDummy :: Either (Int, Type) Data -> DummyData
      getDummy (Left (i, t)) = Dummy {
        current_name = '$' : show i,
        type_sig     = t
      }
      getDummy (Right x) = dummy x

(???) :: [a] -> Int -> Maybe a
(???) y x = if x > length y 
           then Nothing 
           else pure $ y !! (x - 1)
