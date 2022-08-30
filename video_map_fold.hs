{- map -}
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f ls = [f x | x <- ls]

{- filter -}

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f ls = [x | x <- ls, f x]

{- foldr -}

foldr1' :: (t -> t -> t) -> [t] -> t
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)

foldr1'' :: (t-> t -> t) -> [t] -> t
foldr1'' f ls = foldr f (last ls) (init ls)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f b [] = b
foldr' f b (x:xs) = f x (foldr' f b xs)

