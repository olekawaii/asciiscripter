import Data.Bifunctor
import Data.Maybe

main = print $ uwu fns "ffabffcfb"

-- f(f, a(b, f(f, c, f)), b)

data Type = Fn | Val

data RealType = Function RealType RealType | SimpleType

data SimpleType = Number | Gif

fns :: [(Char, [Type])] 
fns = [('f', [Fn, Val, Fn] ), ('a', [Fn, Val]), ('b', [Val]), ('c', [])]

uwu :: [(Char, [Type])] -> [Char] -> String
uwu table jj = let (str,_) = helper [Val] jj in str
  where
    helper :: [Type] -> [Char] -> (String, String)

    helper []        (leftover) = ([], leftover)

    helper x [] = ([],[])
    helper [Fn] (l:eftover) = 
      -- let (a,b) = helper args eftover in
      -- let (a,b) = helper (fromJust $ lookup l table) eftover in
      -- (wrap l a False, b)
      ([l], eftover)

    helper (Fn:args) (l:eftover) = 
      -- let (a,b) = helper args eftover in
      -- let (a,b) = helper (fromJust $ lookup l table) eftover in
      -- first (wrap l a True <>) $ helper args b
      first (([l] <> ", ") <>) $ helper args eftover

    helper [Val] (l:eftover) = 
      let (a,b) = helper (fromJust $ lookup l table) eftover in
      (wrap l a False, b)

    helper (Val:args) (l:eftover) = 
      let (a,b) = helper (fromJust $ lookup l table) eftover in
      first (wrap l a True <>) $ helper args b

    wrap :: Char -> String -> Bool -> String
    wrap c s b = let end = if b then ", " else "" in case s of
      [] -> [c] <> end
      s  -> [c] <> "(" <> s <> ")" <> end
