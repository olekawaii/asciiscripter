{-# Language MultiWayIf, LambdaCase #-}

import Data.Bifunctor (first, second, bimap)
import Data.Maybe (isJust, fromJust, listToMaybe, isNothing, fromMaybe)
import Data.List (find, nub, transpose, sortOn, intercalate)
import Control.Monad (guard)

import Parse

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

format :: String -> [String] -> String
format [] _ = []
format ('%':xs) [] = error xs
format ('%':xs) (a:as) = a <> format xs as
format (x:xs) as = x : format xs as

get :: (Eq a) => a -> BadMap a b -> Maybe b
get a (BadMap list) = fmap snd $ find ((== a) . fst) list

insert :: (Eq a) => a -> b -> BadMap a b -> Maybe (BadMap a b)
insert a b badmap@(BadMap list) = 
  if isJust (get a badmap) 
  then Nothing
  else pure $ BadMap ((a, b) : list)

keys :: BadMap a b -> [a]
keys (BadMap list) = map fst list

new :: BadMap a b
new = BadMap []

main :: IO ()
main = getVideo >>= \(width, height, newGif) -> 
    putStrLn . fst . formatShell 0.2 width height Nothing . pipeline $ newGif

legalNameChars = '_' : ['a'..'z'] <> ['0'..'9'] 

formatShell :: Float -> Int -> Int -> Maybe String -> [Either String Int] 
    -> (ShellScript, ShellScript)
formatShell t wd ht message renderedFrames = case renderedFrames of
  frames  -> (
        init2 <> hideprompt <> initMove <> cleanup <> 
        intro <> alloc <> loop <> body <> done <> "\ncleanup", 
        init2 <> hideprompt <> initMove <> cleanup <> intro <> 
        alloc <> body <> "cleanup"
    )
    where 
      newHelper :: [Either String Int] -> String
      newHelper [] = ""
      newHelper (Left s : xs) = "    draw '" <> s <> "'\n" <> newHelper xs
      newHelper (Right i : xs) = "    sleep " <> show (fromIntegral i * t) 
        <> "\n" <> newHelper xs
      intro = "draw() {\n    printf \"$move_up$1\"\n    sleep " <> 
        show t <> "\n}\n\n"
      alloc = "yes '' | head -n " <> show (ht - 1) <> "\n\n"
      loop = "while true; do\n"
      body = newHelper frames
      done = "done"
  where
    hideprompt = "printf '\\33[?25l'\n\n"
    initMove = "move_up=\"\\33[" <> show (ht - 1) <> "F\"\n\n"
    cleanup = "cleanup() {\n    printf \"$move_up\\33[0J\\33[0m\\33[?25h\"\
        \\n    exit 0\n}\n\ntrap cleanup INT\n\n"
    comment = "#!/bin/sh\n" <> maybe "" (('\n' :) 
        . unlines . map ("# " <>) . lines) message <> "\n"
    sizeCheck = "if [ $(tput cols) -lt " <> show wd <> " -o $(tput lines) -lt " 
        <> show ht <> 
        " ]; then\n    printf \"\\33[91mterminal is too small\\nmust be at least " 
        <> show wd <> " by " <> show ht 
        <> " cells\\33[0m\\n\" >&2\n    exit 1\nfi\n\n" 
    init2 = comment

pipeline :: [[[Character]]] -> [Either String Int]
pipeline input = case reduce2 input of
  [x] -> 
      pure 
    . Left 
    . fromMaybe "" 
    . formatCommands 
    . intercalate [Move Down] 
    . map (map Draw . removeExtraSpaces) 
    $ x
  (x:xs) -> 
      (Left (tail . tail $ renderer x) :) 
    . init 
    . countUp 
    . map formatCommands 
    . reduce 
    $ (x:xs)
  [] -> []

countUp :: [Maybe String] -> [Either String Int]
countUp [] = []
countUp (Just s: xs) = Left s : countUp xs
countUp (Nothing: xs) = Right num : countUp other
    where (num, other) = first ((+ 1) . length) $ span isNothing xs 
      
data Command = Draw Character | Move Dir
data Dir = Down | Next

reduce :: [[[Character]]] -> [[Command]]
reduce [] = []
reduce [x] = pure . intercalate [Move Down] . map (map Draw) $ map removeExtraSpaces x
reduce (x:xs) = reverse . map (uncurry (helper Black)) $ chunksOf2 (x: reverse (x: xs))
  where 
    helper :: Color -> [[Character]] -> [[Character]] -> [Command]
    helper _ [] []                   = []
    helper _ ([[]]) ([[]])           = []
    helper c ([]:xs) ([]:ys)         = Move Down : helper c xs ys
    helper c ((x:xx):xs) ((y:yy):ys) = 
        if x == y 
        then 
          let 
            (same, different) = first (map fst) . span (uncurry (==)) $ zip (x:xx) (y:yy) 
            len        = length same
            leftover   = uncurry (helper c) . bimap (: xs) (: ys) $ unzip different
          in 
          if different /= [] && sum (map (getSize c) same) < 6 
          then map Draw same <> leftover
          else replicate len (Move Next) <> leftover
        else Draw x : helper (fromMaybe c (getColor x)) (xx: xs) (yy: ys)

getSize :: Color -> Character -> Int
getSize c = \case
    Space -> 1
    Character char color -> (if color == c then 0 else 6) + case char of
        '%'  -> 2
        '\'' -> 3
        '\\' -> 2
        _ -> 1

getColor = \case
  Space -> Nothing
  Character _ color -> Just color

formatCommands :: [Command] -> Maybe String
formatCommands x = guard (not $ all isMove x) >> pure (showCommands x Black)
  where
    isMove (Move _) = True
    isMove _ = False

showCommands :: [Command] -> Color -> String
showCommands [] _ = []
showCommands m@(Move x : xs) oldColor = case cleanMove m of 
  (f, []   ) -> show f {next = 0}
  (f, other) -> show f <> showCommands other oldColor
showCommands (Draw x : xs) oldColor = case x of
  Space -> ' ' : showCommands xs oldColor
  c@(Character char color) -> 
    if color == oldColor 
    then clean char <> showCommands xs oldColor 
    else colorChar c <> showCommands xs color 

data FinalMovement = Final {next :: Int, down :: Int} 

instance Show FinalMovement where
  show Final {next = next, down = down} = y <> x
    where 
      y = case down of 
        0 -> "" 
        1 -> "\\n"
        2 -> "\\n\\n"
        _ -> "\\33[" <> show down <> "E" 
      x = if next == 0 then "" else "\\33[" <> show next <> "C"

cleanMove :: [Command] -> (FinalMovement, [Command])
cleanMove x = first (flip makeFinal Final {next = 0, down = 0}) $ helper x
  where 
    helper :: [Command] -> ([Dir], [Command])
    helper (Move x: xs) = first (x:) $ helper xs
    helper x = ([], x)

    makeFinal :: [Dir] -> FinalMovement -> FinalMovement
    makeFinal [] f = f
    makeFinal (Next : xs) f = makeFinal xs f {next = next f + 1, down = down f} 
    makeFinal (Down : xs) f = makeFinal xs f {next = 0, down = down f + 1} 
    
renderer :: [[Character]] -> String
renderer = helper Black . concatMap (Character '\n' Black :)
  where
    helper :: Color -> [Character] -> String
    helper _        [] = "" --"\\n"
    helper oldcolor (Space:xs) = ' ' : helper oldcolor xs
    helper oldcolor (c@(Character char color) : xs) =
      if color == oldcolor || char == '\n'
      then clean char <> helper oldcolor xs
      else  colorChar c <> helper color xs

chunksOf2 :: [a] -> [(a, a)] 
chunksOf2 [] = []
chunksOf2 [a] = []
chunksOf2 (a:b:bs) = (a, b) : chunksOf2 (b:bs)

reduce1 :: [[[Character]]] -> [[[Character]]]
reduce1 = map (map removeExtraSpaces)

reduce2 :: [[[Character]]] -> [[[Character]]]
reduce2 [] = []
reduce2 (x:xs) = if all (== x) xs then [x] else x:xs

removeExtraSpaces :: [Character] -> [Character]
removeExtraSpaces = reverse . remove . reverse
  where
    remove (Space : as) = remove as
    remove as = as

colorChar :: Character -> String
colorChar Space = " "
colorChar (Character s c) = "\\33[9" <> case c of
    Black   -> "0"
    Red     -> "1"
    Green   -> "2"
    Yellow  -> "3"
    Blue    -> "4"
    Magenta -> "5"
    Cyan    -> "6"
    White   -> "7"
  <> "m" <> clean s

clean :: Char -> String
clean '\\' = "\\\\"
clean '\'' = "\\47"
clean '\n' = "\\n"
clean '%'  = "%%"
clean a    = pure a

type ShellScript   = String
type Map a b       = [(a,b)]

data BadMap a b = BadMap [(a, b)]
type Coordinate    = (Int,Int)
data Direction = East | West | North | South deriving (Show, Eq)

