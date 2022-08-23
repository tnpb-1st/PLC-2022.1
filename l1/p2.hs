main = do
   a <- readLn
   b <- readLn
   print (numDiv (a :: Int) (b :: Int))

numDiv :: Integral a => a −> a −> a
numDiv x y
    | x `mod` y == 0 = 1 + numDiv y (x `div` y)
    | otherwise = 0