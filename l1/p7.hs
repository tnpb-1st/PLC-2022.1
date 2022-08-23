main = do
  x <- getLine
  print $ msort (map (read :: String -> Int) (words x))

merge :: [Int] -> [Int] -> [Int]
merge l1 [] = l1
merge [] l2 = l2
merge (x:xs) (y:ys)
    | x <= y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys


msort :: [Int] -> [Int]
msort [] = []
msort [a] = [a]
msort xs = merge (msort(firstHalf xs)) (msort(secondHalf xs))
    where
        firstHalf xs = let {n = length xs} in take (div n 2) xs
        secondHalf xs = let {n = length xs} in drop (div n 2) xs
 