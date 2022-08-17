parseInput str = let [n, k] = map read (words str)
                 in (n, k)
main :: IO()
main = interact $ show . uncurry potencia . parseInput

potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia 0 _ = 0
potencia n k
        | even k = res * res
        | otherwise = n * res * res
        where
            res = potencia n (k `div` 2)