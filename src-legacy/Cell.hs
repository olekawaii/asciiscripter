{-# Language MultiWayIf, LambdaCase #-}

module Cell where

import Color 
import Types 

instance Show Character where
  show Space = " "
  show (Character char color) = colour color (show char)

