main = do
  x <- getLine
  print $ unique (map (read :: String -> Int) (words x))

countElem :: Int -> [Int] -> Int
countElem _ [] = 0
countElem n (x:xs)
    | n == x = 1 + countElem n xs
    | otherwise = countElem n xs

unique :: [Int] -> [Int]
unique [] = []
unique l = [x | x <- l, countElem x l == 1]