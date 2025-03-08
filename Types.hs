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

data NewHeader = NewHeader {
  new_name    :: String,
  typeSig     :: Type,
  block_mark  :: Mark
} deriving Show

instance Show Type where
  show (Type x) = colour Yellow (show x)
  show (Fn a b) = colour Yellow "fn " <> show a <> " " <> show b

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

data ErrorType
  = Delimiter String
  | BadDelimiter String
  | Parse String String String
  | Value String String Int Int
  | Custom String
  | NoMatchingName Name Suggestion 
  | Recursive Name [Name]
  | ArgError String
  | Help
  | CommandArg String Int Int
  | ReallyCustom String
  | BadCommand String Suggestion
  | TypeMismatch DummyData DummyData
  | EmptyGif

instance Show ErrorType where 
  show Help =
    "Usage: ascr [\x1b[33mOPTIONS\x1b[0m] \x1b[35mNAME\x1b[0m <\x1b[36mFILE\x1b[0m>\n\nOptions:\n"
    <> "  \x1b[33m-c\x1b[0m        Don't write to a file\n"
    <> "  \x1b[33m-d\x1b[0m \x1b[36mDIR\x1b[0m    Directory in which to save the gif\n"
    <> "  \x1b[33m-f\x1b[0m \x1b[32mNUM\x1b[0m    Frames per second\n"
    <> "  \x1b[33m-h\x1b[0m        Show this help text\n"
    <> "  \x1b[33m-m\x1b[0m        Past StdIn as a comment into the output script\n"
    <> "  \x1b[33m-q\x1b[0m        Suppress success gif"
  show err = case err of
    TypeMismatch a b
      -> show a
      <> " expected a value of type "
      <> (show . getArg . type_sig) a
      <> ".\nHowever, "
      <> show b
      <> " could never evaluate to "
      <> (show . getArg . type_sig) a
    BadCommand s suggestion
      -> "The command '"
      <> s
      <> "' doesn't exist."
      <> show suggestion
    Delimiter s 
      -> "The delimiter "
      <> colour Blue s 
      <> " did not find the matching "
      <> colour Blue (reverse s) 
      <> " starting" 
    BadDelimiter s 
      -> "Unexpected closing deliminator "
      <> colour Blue s
      <> " found"
    Parse thing expected got
      -> "Couldn't parse " 
      <> thing 
      <> ". Expected " 
      <> expected 
      <> " but got " 
      <> colour Red ("'" <> got <> "'")
    Value thing name expected got
      -> "Couldn't parse " 
      <> thing 
      <> ". Expected " 
      <> show expected
      <> " "
      <> name
      <> " but got " 
      <> colour Red (show got)
    Custom s 
      -> s 
    NoMatchingName a suggestion
      -> "Could not find the gif '"
      <> colour Magenta a
      <> "' in the input files"
      <> show suggestion
    Recursive a l
      -> "The script "
      <> colour Magenta a
      <> " called itself recursively.\n"
      <> foldr1 (\x y -> colour Magenta x <> " -> " <> colour Magenta y) l
    ArgError s
      -> s
      <> ". Check " <> colour Yellow "ascr -h"
    CommandArg c x y
      -> "The command "
      <> colour Green c
      <> " expected "
      <> show x
      <> " arguments but got "
      <> show y
    ReallyCustom x
      -> x
    EmptyGif
      -> "The gif is empty"

instance Show Mark where
  show x = "\x1b[31;1mError\x1b[0m" <> (
    case x of
      File {origin = o, line = l, block = b} 
        -> " at line " 
        <> show (Colored Cyan l) 
        <> " in " 
        <> colour Cyan o
        <> maybe "" (\name -> " (in the definition of " <> colour Magenta name <> ")") b
      Arguments -> " in the \x1b[33marguments\x1b[0m"
      None      -> ""
    ) <> ":\n"
    

data Modifiers = Modifiers {
  target      :: String,
  frameTime   :: Float,
  directory   :: FilePath,
  message     :: Bool,
  quiet       :: Bool,
  check       :: Bool,
  text        :: Bool
} deriving Show

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

