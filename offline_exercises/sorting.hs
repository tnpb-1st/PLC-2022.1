quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [a] = [a]
quicksort (x:xs) = quicksort (lesser x xs) ++ [x] ++ quicksort (greater x xs)
    where
        lesser k b = [a | a <- b, a <= k]
        greater k b = [a | a <- b, a > k]

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [a] = [a]
mergesort ls = merge (mergesort (firstHalf ls)) (mergesort (secondHalf ls))
    where
        firstHalf ls = take (length ls `div` 2) ls
        secondHalf ls = drop (length ls `div` 2) ls
        merge :: [Int] -> [Int] -> [Int]
        merge [] [] = []
        merge a [] = a
        merge [] b = b
        merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]
