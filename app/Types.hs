module Types where

type OrError       = Either Error
type OrToError     = Either (Mark -> Error)
type Body          = (Header, Lines)
type LineNumber    = Int
type NamedLines    = Map Name Lines
type Lines         = Map LineNumber String
type EpicGifData   = Map Name Gif
type Dependencies  = Map Name [Name]
type Size          = (Int, Int)
type Name          = String
type Frame         = Map Coordinate (Colored Char)
type Gif           = [Frame]
type Layer         = Map Coordinate Art
type Coordinate    = (Int,Int)
type Map a b       = [(a,b)]

data Mark = Mark {origin :: FilePath, line :: LineNumber}

data Marked a = Marked Mark a deriving Show

instance Functor Marked where
  fmap f (Marked a b) = Marked a (f b)

data Error 
  = Delimiter  Mark
  | Parse      String String String Mark
  | Custom     String Mark
  | MissingArgs Int
  | NoMatchingName Name
  | Recursive Name

instance Show Error where 
  show err = flip mappend ".\n" $ "\x1b[31mError: \x1b[0m" <> case err of
    Delimiter m 
      -> "The delimiter at was not closed" 
      <> show m 
    Parse thing expected got m
      -> "Couldn't parse " 
      <> thing 
      <> ". Expected " 
      <> expected 
      <> " but got " 
      <> got
      <> show m
    Custom s m 
      -> s 
      <> show m
    MissingArgs n
      -> "Missing Arguments. Expected at least 2 but got "
      <> show n
    NoMatchingName a
      -> "could not find "
      <> a
      <> " in the input files"
    Recursive a
      -> show a
      <> " called itself recursively"

instance Show Mark where
  show Mark {origin = o, line = l} = " at line " <> show l <> " in " <> o

data Header = Header {
  width   :: Int,
  height  :: Int,
  frames  :: Int,
  name    :: String
} deriving Show

data Command 
  = Draw  Layer Int Int Gif 
  | Clear Layer
  | Shift Layer Int Int

data Encoded a = Script a | Drawing a -- for parse

data Art = Giffy Header Gif | Framy Header Frame

data Colored a = Colored Color a
data Color 
  = Black    
  | Red      
  | Green    
  | Yellow   
  | Blue     
  | Magenta  
  | Cyan     
  | White    
  | Transp
  deriving Eq

instance Show a => Show (Colored a) where
  show (Colored Black   s) = "\x1b[30m" <> (init . tail $ show s)
  show (Colored Red     s) = "\x1b[31m" <> (init . tail $ show s)
  show (Colored Green   s) = "\x1b[32m" <> (init . tail $ show s)
  show (Colored Yellow  s) = "\x1b[33m" <> (init . tail $ show s)
  show (Colored Blue    s) = "\x1b[34m" <> (init . tail $ show s)
  show (Colored Magenta s) = "\x1b[35m" <> (init . tail $ show s)
  show (Colored Cyan    s) = "\x1b[36m" <> (init . tail $ show s)
  show (Colored White   s) = "\x1b[37m" <> (init . tail $ show s)
  show (Colored Transp  s) = "\x1b[30m" <> (init . tail $ show s)
