module Types where

type ShellScript   = String
type Map a b       = [(a,b)]
type Dependencies  = Map Name [Name]
type Frame         = [Colored Char]
data Block = Block NewHeader [Marked String] BlockType

data BlockType =  Art Int Int | Script

data NewHeader = NewHeader {
  new_name    :: String,
  typeSig     :: Type,
  block_mark  :: Mark
} -- deriving Show

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

data ComplexError = RealError ErrorType | Func (DummyData -> ErrorType) | Success (DummyData, [DummyData]) 

type Coordinate    = (Int,Int)
type RealGif = [Map Coordinate Character]

data Type = Type SimpleType | Fn Type Type deriving Eq

data SimpleType = Int | Giff | Colour deriving Eq

data Data = Data {
  dummy         :: DummyData,
  currentArgs   :: [Data],
  function      :: [Data] -> ReturnType
}

data DummyData = Dummy {
  current_name   :: String,
  type_sig       :: Type
}

data ReturnType = I Int | G RealGif | C Color -- deriving Show
data Character = Space | Character Char Color deriving Eq

type OrError       = Either Error
type LineNumber    = Int

newtype Suggestion = Suggestion (Maybe String) 
data Mark 
  = Arguments
  | None
  | File {
      origin :: FilePath, 
      line   :: LineNumber, 
      block  :: Maybe Name
    }

data Marked a = Marked Mark a -- deriving Show

data Error = Error {
  errorType :: ErrorType,
  errorMark :: Mark
}

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

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White 
  deriving (Show, Eq)

data Colored a = Colored Color a deriving Eq
type Name = String
