module Format where 

format :: String -> [String] -> String
format [] _ = []
format ('%':xs) [] = error xs
format ('%':xs) (a:as) = a <> format xs as
format (x:xs) as = x : format xs as

