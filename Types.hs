module Types where

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

-- newtype Name = Name String

instance Show SimpleType where
  show Int   = "int"
  show Giff  = "gif"
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

-- instance Show Data where
--   show Data {typeSigniture = t, currentArgs = c, currentName = n} = n <> " : " <> show t

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

