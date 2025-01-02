module Types where

type Map a b       = [(a,b)]
type OrError       = Either Error
type OrToError     = Either (Mark -> Error)
type LineNumber    = Int
type EpicGifData   = Map Name Gif
type Dependencies  = Map Name [Name]
type Name          = String
type Frame         = Map Coordinate (Colored Char)
type Gif           = [Frame]
type Layer         = Map Coordinate Gif
type Coordinate    = (Int,Int)
type Line          = String
type Lines         = [String] --Map LineNumber String

data Mark = Mark {origin :: FilePath, line :: LineNumber}

data Marked a = Marked Mark a deriving Show

instance Functor Marked where
  fmap f (Marked a b) = Marked a (f b)

data Error 
  = Delimiter  String Mark
  | BadDelimiter String Mark
  | Parse      String String String Mark
  | Custom     String Mark
  | MissingArgs Int
  | NoMatchingName Name
  | Recursive Name

instance Show Error where 
  show err = flip mappend ".\n" $ "\x1b[31mError: \x1b[0m" <> case err of
    Delimiter s m 
      -> "The delimiter "
      <> colour Blue s 
      <> " did not find the matching "
      <> colour Blue (reverse s) 
      <> " starting" 
      <> show m 
    BadDelimiter s m 
      -> "Unexpected closing deliminator "
      <> colour Blue s
      <> " was found"
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
      -> "Could not find the name "
      <> colour Magenta a
      <> " in the input files"
    Recursive a
      -> "The script "
      <> colour Magenta a
      <> " called itself recursively"

instance Show Mark where
  show Mark {origin = o, line = l} = 
    " at line " <> 
    show (Colored Cyan l) <> 
    " in " <>
    colour Cyan o

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

data Notated a = Script a | Drawings a -- for parse

instance Functor Notated where
  fmap f (Script a) = Script (f a)
  fmap f (Drawings a) = Drawings (f a)

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
colorCode Transp  = "\x1b[30m"
