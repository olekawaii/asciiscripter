{-# Language MultiWayIf, LambdaCase #-}

module Color where
import Types

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

