module Types where

type Map a b       = [(a,b)]
type OrError       = Either Error
type OrToError     = Either (Mark -> Error)
type LineNumber    = Int
type EpicGifData   = Map Header Gif
type Dependencies  = Map Name [Name]
type Name          = String
type Frame         = [Colored Char]
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
  = Delimiter String Mark
  | BadDelimiter String Mark
  | Parse String String String Mark
  | Custom String Mark
  | NoMatchingName Name
  | Recursive Name
  | ArgError String
  | Help

instance Show Error where 
  show Help =
    "Usage: ascr [\x1b[33mOPTIONS\x1b[0m] \x1b[35mNAME\x1b[0m <\x1b[36mFILE\x1b[0m>\n\nOptions:\n"
    <> "  \x1b[33m-c\x1b[0m        Don't write to a file\n"
    <> "  \x1b[33m-d\x1b[0m \x1b[36mDIR\x1b[0m    Directory in which to save the gif\n"
    <> "  \x1b[33m-f\x1b[0m \x1b[32mNUM\x1b[0m    Frames per second\n"
    <> "  \x1b[33m-h\x1b[0m        Show this help text\n"
    <> "  \x1b[33m-m\x1b[0m        Past StdIn as a comment into the output script\n"
    <> "  \x1b[33m-q\x1b[0m        Suppress success gif"
  show err = flip mappend "." $ "\x1b[31;1mError:\x1b[0m " <> case err of
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
      <> " found"
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
    NoMatchingName a
      -> "Could not find the gif "
      <> colour Magenta a
      <> " in the input files"
    Recursive a
      -> "The script "
      <> colour Magenta a
      <> " called itself recursively"
    ArgError s
      -> s
      <> ". Check " <> colour Yellow "ascr -h"

instance Show Mark where
  show Mark {origin = o, line = l} = 
    " at line " <> 
    show (Colored Cyan l) <> 
    " in " <>
    colour Cyan o

data Modifiers = Modifiers {
  fps         :: Float,
  directory   :: FilePath,
  message     :: Bool,
  quiet       :: Bool,
  check       :: Bool
} deriving Show

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
