{-# Language MultiWayIf, LambdaCase #-}

module Main (main) where

import System.Exit (die)
import System.Process (callCommand)
import System.Environment (getArgs)
import Control.Applicative (liftA2)
import Control.Monad ((<=<), (>=>), guard, unless, when)
import Control.Arrow ((<<<), (>>>), (***), (&&&), (+++), (|||))
import qualified Data.Map.Strict as Map 
import Data.Bifunctor (first, second, bimap)
import Data.Either (fromRight, fromLeft)
import Data.Maybe (fromJust, listToMaybe, isNothing, fromMaybe)
import Data.Tuple (swap)
import Data.List (nub, transpose, sortOn, intercalate)
import Text.Read (readMaybe) 

import Optimization (formatShell, pipeline)
import Cell
import Color
import Library
import Value
import Name
import Error
import Evaluate
import Types

infix 8 ...

main :: IO ()
main = getArgs >>= \arg -> case parseArgs arg defaultMods of
  Left e -> exitWithError e
  Right (mods@Modifiers {
    output = o, target = t, directory = d,
    quiet  = q, check  = c, message   = m
  }, files) ->
    (
      if m
      then pure <$> (putStrLn "enter a message:" >> getContents)
      else pure Nothing
    ) >>= \messageIn ->
    mapM readFile files >>= \fl ->
    case (newGiga (Marked Arguments t) mods . concat . zipWith number files) fl >>= evaluate of
      Left e -> exitWithError e
      Right (I int) -> print int
      Right (G gif) -> case changeFormat gif of
        Left e -> exitWithError Error {errorType = e, errorMark = None}
        Right (width, height, newGif) ->
          let
            file = t <> ".sh"
            shortFile = "short_" <> file
            (fileSh, command) = formatShell mods width height messageIn . pipeline $ newGif
          in
          (
            if width > 80
            then putStrLn "\x1b[33;1mWarning:\x1b[0m video should not be over 80 chars in width"
            else if height > 24
              then putStrLn "\x1b[33;1mWarning:\x1b[0m video should not be over 24 chars in height"
              else pure ()
          ) >>
          writeFile file fileSh >>
          writeFile shortFile command >>
          callCommand ("chmod +x " <> file <> " " <> shortFile) >>
          putStrLn "\x1b[92mcompiled"

dimensions :: RealGif -> Either ErrorType (Int, Int, Int, Int)
dimensions gif = case concatMap (map fst) gif of
  [] -> Left EmptyGif
  g  -> pure $ getDimensions g
    where
      getDimensions :: [Coordinate] -> (Int, Int, Int, Int)
      getDimensions x =
        let
          xCoords = map fst x
          yCoords = map snd x
        in
          (
            minimum xCoords,
            maximum xCoords,
            minimum yCoords,
            maximum yCoords
          )

getMark :: Marked x -> Mark
getMark (Marked m _) = m

defaultMods :: Modifiers
defaultMods = Modifiers {
  frameTime = 0.2,
  directory = ".",
  message   = False,
  quiet     = False,
  check     = False,
  text      = False,
  target    = "main",
  output    = Looping
}

-- check that it's not 0
changeFormat :: RealGif -> Either ErrorType (Int, Int, [[[Character]]])
changeFormat x = dimensions x >>= \(x_min, x_max, y_min, y_max) ->
  let
    chart = liftA2 (flip (,))  [y_max, y_max -1 .. y_min] [x_min..x_max]
    convertFrame :: Map Coordinate Character -> [[Character]]
    convertFrame a =  chunksOf (x_max - x_min + 1) $ map lookupChar chart
      where
        lookupChar want = fromMaybe Space $ lookup want a
  in
  pure (x_max - x_min + 1, y_max - y_min + 1, map convertFrame x) 

parseInt :: String -> String -> Either ErrorType Int
parseInt x y = case readMaybe x of
  Just x  -> pure x
  Nothing -> Left $ Parse y "an int" x

parseName :: String -> Either ErrorType Name
parseName s = if isValidName s then pure s else Left $ Parse "name" "a valid name" s

number :: FilePath -> String -> [Marked String]
number f = zipWith (\x y -> Marked File {origin = f, line = x, block = Nothing} y) [1..] . lines

exitWithError :: Error -> IO ()
exitWithError = die . show

newGiga target mods =
  cleanInput >=> parseChunks >=> \x ->
  findDependencies x {- (map (second (second (markList . map (fmap words)))) x) -} target >>= --TODO
  \d -> fromJust . lookup (unwrap target) <$> win builtinFns d x  

maybeGuard :: (a -> Bool) -> a -> Maybe a
maybeGuard f a = if f a then pure a else Nothing

parseArgs :: [String] -> Modifiers -> OrError (Modifiers, [FilePath])
parseArgs (a:as) m = case a of
  "-h"        -> Left Error {errorType = Help, errorMark = None}
  "-n"        -> getNext a as >>= \(b,cs) -> parseArgs cs     m {target    = b   }
  "-c"        -> parseArgs as m {check     = True}
  "-q"        -> parseArgs as m {quiet     = True}
  "-m"        -> parseArgs as m {message   = True}  
  "-d"        -> getNext a as >>= \(b,cs) -> parseArgs cs     m {directory = b   }  
  -- "-s"        -> parseArgs as m {text      = True}
  "-t"        -> getNext a as >>= \(b,cs) -> case b of
    "vid" -> parseArgs cs m {output = Single}
    "loop" -> parseArgs cs m {output = Looping}
    other -> Left Error {
      errorType = ArgError ("The \x1b[33m-t\x1b[0m flag expected either `gif` or `vid`. Got" <> other),
      errorMark = Arguments
    }
  "-f"        -> getNext a as >>= \(b,cs) -> case readMaybe b >>= maybeGuard (>= 0) of 
    Nothing -> Left Error {
      errorType = ArgError "The \x1b[33m-f\x1b[0m flag expected a positive \x1b[33mNUM\x1b[0m",
      errorMark = Arguments
    }
    Just x  -> parseArgs cs m {frameTime = 1 / x}
  ('-':'-':x) -> Left Error {
    errorType = ArgError $ "Unknown argument '" <> colour Yellow ('-':'-':x) <> "'",
    errorMark = Arguments
  }
  ['-',x]     -> Left Error {
    errorType = ArgError $ "Unknown argument '" <> colour Yellow ['-',x] <> "'",
    errorMark = Arguments
  }
  ('-':x)     -> parseArgs (map (cons '-' . pure) x <> as) m
  _           -> pure (m, a:as)
  where 
    getNext :: String -> [String] -> OrError (String, [String])
    getNext s []   = Left Error {
      errorType = Custom (s <> " expected an argument"),
      errorMark = Arguments
    }
    getNext _ [_]  = Left Error {
      errorType = Custom "Expected a list of files at the end",
      errorMark = Arguments
    }
    getNext _ (x:xs)    = pure (x, xs)
parseArgs _ _ = Left Error {
  errorType = ArgError "Args should end with \x1b[33mNAME <FILE>\x1b[0m",
  errorMark = Arguments
}

cons = (:)

append :: a -> [a] -> [a]
append  x y = y <> [x]

win :: Map Name Data -> Dependencies -> Map Name (NewHeader, [Marked String]) -> OrError (Map Name Data)
win uwu []          _ = pure uwu
win uwu ((n,[]):xs) d = 
  -- let (header@NewHeader {new_name = name}, str) = fromJust $ lookup n d in
  parseBlock (fromJust $ lookup n d) uwu >>= \s -> 
  win ((n,s):uwu) (second (filter (/= n)) <$> xs) d
win uwu (x:xs)      d = win uwu (append x xs) d

unwrap :: Marked a -> a
unwrap (Marked _ a) = a  
    
legalNameChars = '_' : ['a'..'z'] <> ['0'..'9'] 

isValidName :: Name -> Bool
isValidName x = all ($ x) 
  [ flip notElem ["art"]
  , not . null
  , all (`elem` legalNameChars)
  , flip elem ['a'..'z'] . head
  ]

(...) = (.).(.)

stripWhitespace :: String -> String
stripWhitespace = reverse . helper . reverse
  where 
    helper []       = []
    helper (' ':xs) = helper xs
    helper x        = x


shiftAll :: Int -> Int -> Map Coordinate a -> Map Coordinate a
shiftAll x y = map (first ((+ x) *** (+ y)))

insertVal :: Eq a => a -> b -> Map a b -> Map a b
insertVal new val x = (new,val) : filter ((/= new) . fst) x

rotate [] = []
rotate (x:xs) = xs <> [x]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n x  = uncurry ((. chunksOf n) . cons) $ splitAt n x

parseColorLine :: Marked String -> OrError [Maybe Character]
parseColorLine (Marked m x) = uncurry helper $ splitAt (length x `quot` 2) x 
  where
    helper [] [] = pure []
    helper (x:xs) (y:ys) = case x of
      ' ' -> case y of
        '.' -> (Nothing    :) <$> helper xs ys
        '/' -> (Just Space :) <$> helper xs ys
        _   -> Left $ Error {
          errorType = Custom (
            "the space character marked `" <> 
            show y <> 
            "` should be marked transparent or filled"
          ),
          errorMark = m
        }
      char -> case y of
        '.' -> Left $ Error {
          errorType = Custom ("non-space character `" <> show x <> "` marked transparent"),
          errorMark = m
        }
        '/' -> Left $ Error {
          errorType = Custom ("non-space character `" <> show x <> "` marked as a space"),
          errorMark = m
        }
        _   -> parseColor >>= \color -> (Just (Character x color) :)  <$> helper xs ys
          where 
            parseColor = case y of
              '0' -> pure Black 
              '1' -> pure Red
              '2' -> pure Green
              '3' -> pure Yellow
              '4' -> pure Blue
              '5' -> pure Magenta
              '6' -> pure Cyan
              '7' -> pure White
              x   -> Left $ Error {
                errorType = Custom ("bad color " <> show (x:xs) <> "\n" <> show (y:ys)),
                errorMark = m
              }

findSimilarName :: Name -> [Name] -> Suggestion
findSimilarName name = Suggestion . listToMaybe . sortOn (cancel name)

cancel :: Name -> Name -> Int
cancel []     name = length name 
cancel (n:ns) name = let cut = rm n name in cancel ns cut + if cut == name then 1 else -1
  where 
    rm x []     = []
    rm x (y:ys) = if x == y then ys else y : rm x ys

parseType :: [String] -> Either ErrorType Type
parseType x = parseTypeSigniture x >>= \case
  (t,[]) -> pure t
  _      -> Left $ Custom "Trailing words after the type signiture"
parseTypeSigniture :: [String] -> Either ErrorType (Type, [String])
parseTypeSigniture ("frames":xs) = pure (Type Giff, xs)
parseTypeSigniture ("color":xs) = pure (Type Colour, xs)
parseTypeSigniture ("int":xs) = pure (Type Int,  xs)
parseTypeSigniture ("fn":xs)  = 
  parseTypeSigniture xs >>= \(a,b) ->
  parseTypeSigniture b  >>= \(c,d) ->
  pure (Fn a c, d)
parseTypeSigniture x = Left $ Custom ("Unknown type: " <> show x) 

cleanInput :: [Marked String] -> OrError [Marked String]
cleanInput [] = pure []
cleanInput (Marked m s : xs) = case trim s of
  ""    -> cleanInput xs
  "---" -> findClosing xs >>= cleanInput
  "<o>" -> Left Error {
    errorType = BadDelimiter "<o>",
    errorMark = m
  }
  _     -> (Marked m (stripWhitespace s) :) <$> cleanInput xs
  where
    findClosing [] = pure []
    findClosing (Marked m a : as) = case trim a of
      "<o>" -> pure as
      "---" -> Left Error {
        errorType = BadDelimiter "---",
        errorMark = m
      }
      _     -> findClosing as
    
trim :: String -> String
trim = reverse . trimhelper . reverse . trimhelper
  where
    trimhelper [] = []
    trimhelper (' ':xs) = trimhelper xs
    trimhelper x = x

-- parseBlock :: NewHeader -> [Marked String] -> Map Name Data -> OrError Data
parseBlock :: (NewHeader, [Marked String]) -> Map Name Data -> OrError Data
parseBlock (header, lns) table =
  let
        output = parseScript2 header table lns
  in
  output >>= \fn -> pure Data {
      dummy = Dummy {
        current_name = new_name header,
        type_sig     = typeSig header
      },
      currentArgs   = [],
      function      = fn
    }

markList :: [Marked [a]] -> [Marked a]
markList = concatMap (\(Marked m x) -> map (Marked m) x)


argTypes :: Type -> [Type]
argTypes (Fn a b) = a : argTypes b
argTypes _        = []

numberOfArgs :: Type -> Int
numberOfArgs (Type _) = 0
numberOfArgs (Fn _ b) = 1 + numberOfArgs b

result :: Type -> Type
result (Type x) = Type x
result (Fn _ b) = result b

splitBlock :: [Marked String] -> OrError ([Marked Name], Maybe (Int, Int, [Marked String]))
splitBlock [] = pure ([], Nothing)
splitBlock (Marked _ ('-':'-':_): xs) = splitBlock xs
splitBlock (Marked m s : xs) = helper (words s) >>= \(ws, art) -> case art of
  Just (x, y) -> pure (ws, pure (x, y, filter (not . isArtComment x . unwrap) xs))
  Nothing -> first (ws <>) <$> splitBlock xs
  where 
    helper :: [String] -> OrError ([Marked Name], Maybe (Int, Int))
    helper [] = pure ([], Nothing)
    helper ("--" : other) = pure ([], Nothing)
    helper ("art" : other) = case traverse readMaybe other of
      Just [a, b] -> pure ([], Just (a, b))
      _ -> mkError m . Left $ Custom "bad art syntax"
    helper (w : other) = first (Marked m w :) <$> helper other

-- Header only needed for the error message TODO
findDependencies :: Map Name (NewHeader, [Marked String]) -> Marked Name -> OrError (Map Name [Name])
findDependencies table = fmap nub . getDependencies []
  where 
    getDependencies :: [Name] -> Marked Name -> OrError (Map Name [Name])
    getDependencies used (Marked m target) = if
      | elem target (map fst builtinFns) -> pure []
      | elem target used -> Left Error {
          errorType = Recursive target (reverse $ target : used),
          errorMark = None
        }
      | otherwise -> case snd <$> lookup target table :: Maybe [Marked String] of 
        Nothing -> Left Error {
          errorType = NoMatchingName target (findSimilarName target (map fst table)),
          errorMark = m
        }
        Just lines  -> extract . fst <$> splitBlock lines >>= \x -> cons (target, map unwrap x) . concat <$> traverse (getDependencies (target:used)) x
        
    extract :: [Marked String] -> [Marked Name]
    extract lns = 
      helper lns
      -- helper . markList . map (fmap words) $ lns
        where
          helper :: [Marked String] -> [Marked String]
          helper = filter (\(Marked m x) -> notElem x (map fst builtinFns) && not (all (`elem` ('$':nums)) x))

parseChunks :: [Marked String] -> OrError (Map Name (NewHeader, [Marked String]))
parseChunks [] = pure []
parseChunks (Marked m ('-':'-':_) : xs) = parseChunks xs 
parseChunks all@(Marked m x : xs) = 
  parseHeader (Marked m x)  >>= \header -> 
  find_leftover xs           >>= \(inside, other) ->
    case map (flip addMarkBlock (new_name header)) inside of
      [] -> Left Error {errorType = MissingBody, errorMark = block_mark header}
      all@(Marked m ln:lns) ->
        let
          lines = case traverse readMaybe $ words ln of
            Just [a, b] -> (filter (not . isArtComment a . unwrap) lns)
            Nothing     -> all
        in
        ((new_name header, (header, lines)):) <$> parseChunks other
  where 
    find_leftover :: [Marked String] -> OrError ([Marked String],[Marked String])
    find_leftover [] = Left Error {
      errorType = Parse "code block" "end" "EOF",
      errorMark = m
    }
    find_leftover (Marked m a : as) = case trim a of
      "end" -> pure ([],as)
      _     -> first (Marked m a :) <$> find_leftover as

isComment :: String -> Bool
isComment ('-':'-':_) = True
isComment _ = False

isArtComment :: Int -> String -> Bool
isArtComment width ('-':'-':' ':xs) = length xs + 3 `mod` width * 2 /= 0
isArtComment _ "--" = True
isArtComment _ _ = False

addMarkBlock :: Marked a -> Name -> Marked a
addMarkBlock (Marked m s) name = Marked m {block = pure name} s

mkError :: Mark -> Either ErrorType a -> Either Error a
mkError _ (Right x) = Right x
mkError m (Left e) = Left Error {
  errorType = e,
  errorMark = m
}

parseHeader :: Marked String -> OrError NewHeader
parseHeader (Marked m s) = case words s of 
  (name:":":t) -> mkError m $ 
    parseName name  >>= \name -> 
    parseType t     >>= \sig -> 
    pure NewHeader {
      new_name   = name,
      typeSig    = sig,
      block_mark = m
    }
  _ -> Left Error {
    errorType = Custom "A header has the syntax `name : type`",
    errorMark = m
    }

nums = '-' : ['0'..'9']

surround :: String -> String
surround x = case words x of
  [_] -> x
  _   -> "(" <> x <> ")"

isPossible :: Type -> Type -> Bool
isPossible w f@(Fn a b) = w == f || isPossible w b
isPossible w got        = w == got
  
evaluateRealTypes :: Type -> [Marked DummyData] -> Mark -> Either Error DummyData
evaluateRealTypes want x body_mark= case evaluateTypes want x of
  RealError x    -> Left x
  Success (Marked _ x,[]) -> Right x
  Success (Marked m x,xs) -> mkError m . Left $ Custom (show x <> " did not expect any arguments")
  Func f         -> mkError body_mark . Left $ Custom "TypeMismatch in the first argument"

data ComplexError = 
  RealError Error | 
  Func (Marked DummyData -> Error) | 
  Success (Marked DummyData, [Marked DummyData]) 

evaluateTypes :: Type -> [Marked DummyData] -> ComplexError
evaluateTypes want [] = Func (\(Marked m name) -> Error {
  errorType = Custom (show name <> " is missing arguments"),
  errorMark = m
})
evaluateTypes (Type a) (Marked m x@Dummy {type_sig = Type b} : xs) = 
  if a == b then Success (Marked m x,xs) else Func $ \(Marked _ d) -> Error {
    errorType = TypeMismatch d x,
    errorMark = m
  }
evaluateTypes want (Marked m x@Dummy {type_sig = tp} : xs) = if
  | tp == want -> Success (Marked m x,xs)
  | not (isPossible want tp) -> Func $ \(Marked m d) -> Error {
      errorType = TypeMismatch d x,
      errorMark = m
    }
  | otherwise  -> case tp of
    Type a -> error (show want) --Func (flip TypeMismatch want) -- pure (x,xs)
    Fn a b -> case evaluateTypes a xs of
      RealError x             -> RealError x
      Func f                  -> RealError (f (Marked m x))
      Success (arg, leftover) -> 
        case applyDummy x (unwrap arg) of
          Left x -> error "evaluateTypes "
          Right d -> case evaluateTypes want (Marked m d:leftover) of 
            RealError x             -> RealError x
            Func f                  -> RealError (f (Marked m d))
            Success (arg, leftover) -> Success (arg, leftover)

applyDummy :: DummyData -> DummyData -> OrError DummyData
applyDummy Dummy {type_sig = Type _} _ = Left Error {
  errorType = Custom "applied value to non-function",
  errorMark = None
}
applyDummy 
  Dummy {type_sig = Fn a b, current_name = name1} 
  Dummy {type_sig = x, current_name = name2} =
    if x /= a 
    then Left Error {
      errorType = Custom "applied value to non-function",
      errorMark = None
    }
    else pure Dummy {
      current_name = name1 <> " " <> surround name2,
      type_sig = b
    }

unsafeEval :: Type -> [Data] -> Data
unsafeEval = fst ... unsafeEvaluateExpression
  where
    unsafeEvaluateExpression :: Type -> [Data] -> (Data, [Data])
    unsafeEvaluateExpression (Type a) (x@Data {dummy = Dummy {type_sig = Type _}} :xs) = (x,xs)  
    unsafeEvaluateExpression want (x@Data {dummy = Dummy {type_sig = Fn a b}} : xs) = 
      if Fn a b == want then (x,xs) else
        let (arg, leftover) = unsafeEvaluateExpression a xs in
        unsafeEvaluateExpression want (unsafeApplyFn x arg : leftover) 


unsafeApplyFn :: Data -> Data -> Data
unsafeApplyFn 
  Data {dummy = d@Dummy {type_sig = Fn a b}, currentArgs = args, function = f} 
  arg@Data {dummy = d2} = 
  Data {
    dummy = case applyDummy d d2 of
      Right x -> x
      Left _ -> error "can't happen",
    currentArgs    = args <> [arg],
    function       = f
  } 

-- TODO return Marked for typechecking
parseScript2 :: NewHeader -> Map Name Data -> [Marked String] -> OrError ([Data] -> ReturnType)
parseScript2 header@NewHeader {typeSig = tp, block_mark = block_mark} table xs = 

  splitBlock xs >>= \(script, art) -> 
  let 

    helperParseScript :: [Marked String] -> OrError [Marked (Either (Int, Type) Data)]
    helperParseScript [] = case art of
      Nothing     -> pure []
      Just (x, y, lns) -> parseGif header x y lns >>= \vid -> 
        pure .  
        pure . 
        Marked block_mark . 
        Right $ Data {
          dummy = Dummy {
            current_name = new_name header,
            type_sig = Type Giff
          },
          currentArgs = [],
          function = vid
        }
    helperParseScript (Marked m ('$':num) : xs) = case readMaybe num of
      Nothing -> Left Error {
        errorType = Custom "$ must be preceded by a number",
        errorMark = m
      }
      Just x  -> case arguments ??? x of
        Nothing -> 
          Left $ Error {
            errorType = Custom "index is greater than the number of arguments",
            errorMark = m
          }
        Just t ->
          (Marked m (Left (x,t)) :) <$> helperParseScript xs
    helperParseScript (Marked m x : xs) = 
      case readMaybe x of  -- all (`elem` nums) x 
        Just num ->             
          let 
            d = Data {
              dummy = Dummy {
                current_name  = x,
                type_sig      = Type Int
              },
              currentArgs   = [],
              function      = const (I num)
            }
          in cons (Marked m (Right d)) <$> helperParseScript xs
        Nothing -> case lookup x table of
          Nothing -> Left $ Error {
            errorType = Custom "variable not in scope",
            errorMark = m
          }
          Just x  -> cons (Marked m (Right x)) <$> helperParseScript xs

  in
  helperParseScript script >>= \newTable -> 
  (evaluateRealTypes (result tp) (map (fmap getDummy) newTable) block_mark :: Either Error DummyData) >>
  pure (\args -> 
      let
        matcher (Marked _ x) = case x of
          Left (i,_)  -> args !! (i - 1)
          Right x     -> x

      in
      case evaluate . unsafeEval (result tp) $ map matcher newTable of
        Right x -> x
        Left x -> error "can't happen 2"
    )
    where
      arguments = argTypes tp
      

      getDummy :: Either (Int, Type) Data -> DummyData
      getDummy (Left (i, t)) = Dummy {
        current_name = '$' : show i,
        type_sig     = t
      }
      getDummy (Right x) = dummy x

(???) :: [a] -> Int -> Maybe a
(???) y x = if x > length y 
            then Nothing 
            else pure $ y !! (x - 1)

parseGif :: NewHeader -> Int -> Int -> [Marked String] -> OrError ([Data] -> ReturnType)
parseGif header w h lns = 
  if mod (length lns) h /= 0 
  then Left Error {
    errorType = Custom $
      "A gif's number of lines should be divisible by the header's height.\nYou have "
      <> show (length lns) <> " lines." <> show (length lns),
    errorMark = block_mark header
  }
  else traverse validateStrings (chunksOf h lns) >>= \gif -> 
      pure $ \_ ->
      G . map (removeTransp . zip (liftA2 (flip (,)) [h -1 , h -2 .. 0] [0 .. w - 1])) $ concat gif
  where 
    removeTransp :: Map Coordinate (Maybe Character) -> Map Coordinate Character
    removeTransp [] = []
    removeTransp ((_, Nothing):xs) = removeTransp xs
    removeTransp ((coord, Just char):xs) = (coord, char) : removeTransp xs
        
    validateStrings :: [Marked String] -> OrError [[Maybe Character]]
    validateStrings [] = pure []
    validateStrings (x:xs) = firstStrLen >>= helper (x:xs) >>= traverse (fmap concat. traverse parseColorLine) . map (\(Marked m v) -> map (Marked m) (chunksOf (w * 2) v)) >>=
      pure . map concat . chunksOf h . rearange
      where
        firstStrLen = let lnlen = length . stripWhitespace $ unwrap x in
          if lnlen `mod` w == 0
          then pure lnlen
          else Left Error {
            errorType = Custom $
              "A gif's chars per line should be divisible by the header's width.\nYou have "
              <> show lnlen <> " chars.",
            errorMark = getMark x
          }

        rearange :: [[Maybe Character]] -> [[Maybe Character]]
        rearange = concat . transpose . map (chunksOf w)

        helper :: [Marked String] -> Int -> OrError [Marked String]
        helper [] _ = pure []
        helper ((Marked m x):xs) n = let len = length x in
          if len /= n
          then Left Error {
            errorType = Value "line" "characters" n len,
            errorMark = m
          }
          else cons (Marked m x) <$> helper xs n
