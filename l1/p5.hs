{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
main = do
  x <- getLine
  y <- getLine
  print $ merge (map (read :: String -> Int) (words x)) (map (read :: String -> Int) (words y))

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge a [] = a
merge [] b = b
merge (ha:ta) (hb:tb)
  | ha <= hb = ha : merge ta (hb:tb)
  | otherwise = hb : merge (ha:ta) tb
  where
    res = []
