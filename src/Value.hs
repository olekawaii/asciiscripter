{-# Language MultiWayIf, LambdaCase #-}

module Value where

import Color
import Cell
import Types

instance Show DummyData where
  show Dummy {current_name = name, type_sig = tp} = colour Magenta name <> " : " <> show tp


instance Show SimpleType where
  show Int    = "int"
  show Giff   = "frames"
  show Colour = "color"

instance Show Type where
  show (Type x) = colour Blue (show x)
  show (Fn a b) = colour Blue "fn " <> show a <> " " <> show b

