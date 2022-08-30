main = do
  n <- readLn
  x <- getLine
  print $ remDiv (n :: Int) (words x)
    
take' :: Int -> [t] -> [t]
take' _ [] = []
take' n (x:xs)
    | n == 0 = []
    | otherwise = x : take' (n-1) xs

takeLast :: Int -> [t] -> [t]
takeLast _ [] = []
takeLast 0 _ = []
takeLast n (x:xs)
    | length (x:xs) > n = takeLast n xs
    | otherwise = x:xs

remDiv:: (Eq t) => Int -> [t] -> ([t],[t])
remDiv 0 ys = ([],ys)
remDiv _ [] = ([], [])
remDiv n l = (take' (n - 1) l , takeLast (length l - n) l )