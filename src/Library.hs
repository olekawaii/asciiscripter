{-# Language MultiWayIf, LambdaCase #-}

module Library where

import Value 
import Color
import Cell
import Name
import Types
import Evaluate

import Control.Arrow

builtinFns :: Map Name Data 
builtinFns = [
    ("move"        , movef        ),
    ("reverse"     , reversef     ),
    ("skip"        , skipf        ),
    ("join"        , joinf        ),
    ("slow"        , slowf        ),
    ("null"        , nullf        ),
    ("anchor"      , anchorf      ),
    ("seq"         , seqf         ),
    ("take"        , takef        ),
    ("tail"        , tailf        ),
    ("frame_count" , frame_countf ),
    ("dye"         , dyef         ),
    ("black"       , black        ),
    ("red"         , red          ),
    ("green"       , green        ),
    ("yellow"      , yellow       ),
    ("blue"        , blue         ),
    ("magenta"     , magenta      ),
    ("cyan"        , cyan         ),
    ("white"       , white        )
  ]
  where 
    tailf = Data {
      dummy = Dummy {
        current_name = "tail",
        type_sig     = Fn (Type Giff) (Type Giff)
      },
      currentArgs = [],
      function = \[a] ->
        let
          Right (G x) = evaluate a
        in
        G (tail x)

    }
    movef = Data {
      dummy = Dummy {
        current_name   = "move",
        type_sig = Fn (Type Int) (Fn (Type Int) (Fn (Type Giff) (Type Giff)))
      },
      currentArgs   = [],
      function      = \[a, b, c] -> 
        let 
          Right (I x) = evaluate a 
          Right (I y) = evaluate b
          Right (G z) = evaluate c
        in
        G $ map (map (\((f,g),thing) -> ((f + x, g + y), thing))) z
    }

    reversef = Data {
      dummy = Dummy {
        current_name   = "reverse",
        type_sig = Fn (Type Giff) (Type Giff)
      },
      currentArgs   = [],
      function      = \[a] ->
        let Right (G x) = evaluate a in
        G $ reverse x
    }

    skipf = Data {
      dummy = Dummy {
        current_name   = "skip",
        type_sig = Fn (Type Int) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[a,b] ->
        let 
          Right (I x) = evaluate a 
          Right (G y) = evaluate b
          
          toTake :: Int -> Int -> Int
          toTake x y = if x <= y then x else toTake (x - y) y
          
          (fst, snd)  = splitAt (toTake x (length y)) y
        in G $ snd <> fst
    }

    joinf = Data {
      dummy = Dummy {
        current_name   = "join",
        type_sig = Fn (Type Giff) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[a,b] ->
        let
          Right (G x) = evaluate a
          Right (G y) = evaluate b

          frames = length x `lcm` length y in 
        G . map (uncurry mappend) . take frames $ zip (cycle x) (cycle y)
    }

    nullf = Data {
      dummy = Dummy {
        current_name   = "null",
        type_sig = Type Giff
      },
      currentArgs   = [],
      function      = \_ ->
        G [[]]
    }

    anchorf = Data {
      dummy = Dummy {
        current_name   = "anchor",
        type_sig = Type Giff
      },
      currentArgs   = [],
      function      = \_ ->
        G [[((0, 0), Space)]]
    }

    slowf = Data {
      dummy = Dummy {
        current_name   = "slow",
        type_sig = Fn (Type Int) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[a,b] ->
        let 
          Right (I x) = evaluate a
          Right (G y) = evaluate b
        in G . concatMap (replicate x) $ y
    } 

    seqf = Data {
      dummy = Dummy {
        current_name   = "seq",
        type_sig = Fn (Type Giff) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[a,b] ->
        let
          Right (G x) = evaluate a
          Right (G y) = evaluate b
        in 
        G $ x <> y
    }

    takef = Data {
      dummy = Dummy {
        current_name   = "take",
        type_sig = Fn (Type Int) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[a,b] ->
        let 
          Right (I x) = evaluate a
          Right (G y) = evaluate b
        in 
        G . take x $ cycle y
    }

    frame_countf = Data {
      dummy = Dummy {
        current_name   = "frame_count",
        type_sig = Fn (Type Giff) (Type Int)
      },
      currentArgs   = [],
      function      = \[a] ->
        let
          Right (G x) = evaluate a
        in I $ length x
    }

    dyef = Data {
      dummy = Dummy {
        current_name   = "dye",
        type_sig = Fn (Type Colour) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[a,b] ->
        let 
          Right (C x) = evaluate a
          Right (G y) = evaluate b
          recolor Space = Space
          recolor (Character s _) = Character s x 
        in G $ map (map (second recolor)) y
    }

    createColor :: Color -> Data
    createColor x = Data {
      dummy = Dummy {
        current_name = show x,
        type_sig     = Type Colour
      },
      currentArgs   = [],
      function      = const (C x)
    }

    black   = createColor Black   
    red     = createColor Red     
    green   = createColor Green   
    yellow  = createColor Yellow  
    blue    = createColor Blue    
    magenta = createColor Magenta 
    cyan    = createColor Cyan    
    white   = createColor White

