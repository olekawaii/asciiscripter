module Types where

type Body          = (Header, RawData)
type LineNumber    = Int
type NamedRawData  = Map Name RawData
type RawData       = Map LineNumber String
type EpicGifData   = Map Name Gif
type Dependencies  = Map Name [Name]
type Size          = (Int, Int)
type Name          = String
type Frame         = Map Coordinate (Colored Char)
type Gif           = [Frame]
type Layer         = Map Coordinate Art
type Coordinate    = (Int,Int)
type Map a b       = [(a,b)]
type FilePath      = String

data Error 
  = Delimiter  String  LineNumber
  | Parse      String  String String LineNumber
  | Custom     String  LineNumber

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

data Colored a
  = Black    a
  | Red      a
  | Green    a
  | Yellow   a
  | Blue     a
  | Magenta  a
  | Cyan     a
  | White    a

instance Show a => Show (Colored a) where
  show (Black   s) = "\x1b[30m" <> show s
  show (Red     s) = "\x1b[31m" <> show s
  show (Green   s) = "\x1b[32m" <> show s
  show (Yellow  s) = "\x1b[33m" <> show s
  show (Blue    s) = "\x1b[34m" <> show s
  show (Magenta s) = "\x1b[35m" <> show s
  show (Cyan    s) = "\x1b[36m" <> show s
  show (White   s) = "\x1b[37m" <> show s
