main :: IO ()
main = do
  x <- getLine
  print $ metade (map (read :: String -> Int) (words x))

metade :: (Integral a) => [a] -> ([a],[a])
metade [] = ([],[])
metade l = (metadePar 0 l [], metadeImpar 0 l [])

metadePar :: (Integral a) => a -> [a] -> [a] -> [a]
metadePar _ [] evenList = evenList
metadePar cur (x:xs) evenList
    | even cur = metadePar (cur+1) xs (evenList ++ [x])
    | otherwise = metadePar (cur+1) xs evenList

metadeImpar :: (Integral a) => a -> [a] -> [a] -> [a]
metadeImpar _ [] oddList = oddList
metadeImpar cur (x:xs) oddList
    | odd cur = metadeImpar (cur+1) xs (oddList ++ [x])
    | otherwise = metadeImpar (cur+1) xs oddList