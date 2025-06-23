{-# Language MultiWayIf, LambdaCase #-}

module Error where

import Color
import Types 
import Format
import Value

instance Show Suggestion where
  show (Suggestion Nothing) = ""
  show (Suggestion (Just x)) = ". Did you mean " <> colour Green x <> "?"

instance Functor Marked where
  fmap f (Marked a b) = Marked a (f b)

instance Show Error where
  show Error {errorType = t, errorMark = m} = show m <> show t

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
    
getArg (Fn a _) = a
