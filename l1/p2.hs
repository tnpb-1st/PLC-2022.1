{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
main = do
  x <- getLine
  y <- getLine
  print $ merge (map (read :: String -> Int) (words x)) (map (read :: String -> Int) (words y))

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [a] [] = [a]
merge [] [a] = [a]
merge (h1:t1) (h2:t2) = [lower h1 h2] ++ [greater h1 h2] ++ merge t1 t2
  where
    lower a b = if a <= b then a else b
    greater a b = if a >= b then a else b
