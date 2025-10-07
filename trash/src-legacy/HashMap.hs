module HashMap where

import Types
import Data.List
import Data.Maybe

get :: (Eq a) => a -> HashMap a b -> Maybe b
get a (HashMap list) = fmap snd $ find ((== a) . fst) list

insert :: (Eq a) => a -> b -> HashMap a b -> Maybe (HashMap a b)
insert a b hashmap@(HashMap list) = 
  if isJust (get a hashmap) 
  then Nothing
  else pure $ HashMap ((a, b) : list)

keys :: HashMap a b -> [a]
keys (HashMap list) = map fst list

new :: HashMap a b
new = HashMap []
