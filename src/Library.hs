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
    ("loop"        , loop         ),
    ("reverse"     , reversef     ),
    ("join"        , joinf        ),
    ("slow"        , slowf        ),
    ("null"        , nullf        ),
    ("anchor"      , anchorf      ),
    ("seq"         , seqf         ),
    ("take"        , takef        ),
    -- ("tail"        , tailf        ),
    ("frame_count" , frame_countf ),
    ("dye"         , dyef         ),
    ("black"       , black        ),
    ("red"         , red          ),
    ("green"       , green        ),
    ("yellow"      , yellow       ),
    ("blue"        , blue         ),
    ("magenta"     , magenta      ),
    ("cyan"        , cyan         ),
    ("white"       , white        ),
    ("east"        , east         ),
    ("north"       , north        ),
    ("south"       , south        ),
    ("west"        , west         ),
    ("shift"       , shift        ),
    ("rotate"      , rotatef      ),
    ("do"          , dof          )
  ]
  where 
    loop = Data {
      dummy = Dummy {
        current_name = "loop",
        type_sig     = Fn (Type Giff) (Type Giff)
      },
      currentArgs = [],
      function = \[a] ->
        let
          Right (G x) = evaluate a
        in
        G $ case x of 
          [] -> []
          [a] -> [a]
          [a,b] -> [a,b]
          as -> as <> tail (reverse (tail as))
    }

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

    shift = Data {
      dummy = Dummy {
        current_name   = "shift",
        type_sig = Fn (Type Direction) (Fn (Type Giff) (Type Giff))
      },
      currentArgs   = [],
      function      = \[b, c] -> 
        let 
          Right (D y) = evaluate b
          Right (G z) = evaluate c
        in
        G $ map (map (\((f,g),thing) -> (
          case y of 
            East  -> (f + 1, g)
            North -> (f, g + 1)
            South -> (f, g - 1)
            West  -> (f - 1, g)
        , thing))) z
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

    rotatef = Data {
      dummy = Dummy {
        current_name   = "rotate",
        type_sig = Fn (Type Giff) (Type Giff)
      },
      currentArgs   = [],
      function      = \[b] ->
        let 
          Right (G y) = evaluate b
        in 
          G (case y of
            [] -> []
            (x:xs) -> xs <> [x]
          )
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

    createDirection :: Direction -> Data
    createDirection x = Data {
      dummy = Dummy {
        current_name = show x,
        type_sig     = Type Direction
      },
      currentArgs   = [],
      function      = const (D x)
    }

    north = createDirection North
    east = createDirection East
    south = createDirection South
    west = createDirection West

    dof :: Data
    dof = Data {
      dummy = Dummy {
        current_name = "do",
        type_sig     = Fn (Type Int) (Fn (Fn (Type Giff) (Type Giff)) (Fn (Type Giff) (Type Giff)))
      },
      currentArgs = [],
      function    = \[a, b, c] ->
        let 
          Right (I number) = evaluate a

          helper 0 = c
          helper x = unsafeApplyFn b (helper (x - 1))
          
          Right (G output) = evaluate (helper number)
        in
          (G output)
    }
          



    -- frame_countf = Data {
    --   dummy = Dummy {
    --     current_name   = "frame_count",
    --     type_sig = Fn (Type Giff) (Type Int)
    --   },
    --   currentArgs   = [],
    --   function      = \[a] ->
    --     let
    --       Right (G x) = evaluate a
    --     in I $ length x
    -- }
