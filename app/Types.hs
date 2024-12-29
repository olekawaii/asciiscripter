module Types where

type OrError       = Either Error
type Body          = (Header, Lines)
type LineNumber    = Int
type NamedLines  = Map Name Lines
type Lines       = Map LineNumber String
type EpicGifData   = Map Name Gif
type Dependencies  = Map Name [Name]
type Size          = (Int, Int)
type Name          = String
type Frame         = Map Coordinate (Colored Char)
type Gif           = [Frame]
type Layer         = Map Coordinate Art
type Coordinate    = (Int,Int)
type Map a b       = [(a,b)]

data Origin = File FilePath | StdIn deriving Show
data Mark = Mark {origin :: Origin, line :: LineNumber} deriving Show

data Marked a = Marked Mark a deriving Show

instance Functor Marked where
  fmap f (Marked a b) = Marked a (f b)

data Error 
  = Delimiter  String LineNumber
  | Parse      String String String LineNumber
  | Custom     String LineNumber

instance Show Error where 
  show err = flip mappend ".\n" $ "\x1b[31mError: \x1b[0m" <> case err of
    Delimiter s l 
      -> "There is an incomplete " 
      <> show s 
      <> " at line " 
      <> show l 
    Parse thing expected got l
      -> "Couldn't parse " 
      <> thing 
      <> ". Expected " 
      <> expected 
      <> " but got " 
      <> got
      <> " at line "
      <> show l
    Custom s l 
      -> s 
      <> " at line " 
      <> show l


data Header = Header {
  width   :: Int,
  height  :: Int,
  frames  :: Int,
  name    :: String
} deriving Show

data Command 
  = Draw  Layer Int Int Frame 
  | Play  Layer Int Int Gif
  | Clear Layer
  | Shift Layer Int Int

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
