map' :: (a -> b) -> [a] -> [b]
map' f ls = [f x | x <- ls]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f ls = [x | x <- ls, f x]

{- 1. Write three line-by-line calculations of doubleAll [2,1,7] using the three
different definitions of doubleAll that use a list comprehension, primitive
recursion and map. -}

doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs

doubleAll' :: [Integer] -> [Integer]
doubleAll' ls = [2*x | x <- ls]

doubleAll'' :: [Integer] -> [Integer]
doubleAll'' ls = map' doubleInt ls
    where
        doubleInt x = 2 * x

{- 6. Give definitions of functions to take a list of integers, ns, and
• return the list consisting of the squares of the integers in ns;
• return the sum of squares of items in ns;
• check whether all items of the list are greater than zero. -}

square :: Integer -> Integer
square x = x * x

f1 :: [Integer] -> [Integer]
f1 = map' square

f2 :: [Integer] -> Integer
f2 [] = 0
f2 (x:xs) = x + f2 xs

greaterZero :: Integer -> Bool
greaterZero x = x > 0

f3 :: [Integer] -> Bool
f3 [] = True
f3 ls =  replicate (length ls) True == map' greaterZero ls

{- Fibonacci com map -}

fibonacci :: Int -> Integer
fibonacci = (map fib [0 ..] !!)
    where
        fib 0 = 0
        fib 1 = 1
        fib n = fibonacci(n-2) + fibonacci(n-1)

{- fastFib -}

fibStep :: (Integer,Integer) -> (Integer,Integer)
fibStep (x,y) = (y, x+y)

fibPair :: Int -> (Integer,Integer)
fibPair 0 = (0,1)
fibPair n = fibStep(fibPair(n-1))

fibonacci' :: Int -> Integer
fibonacci' n = fst (fibPair n)

{- Strong fib -}

fibonacci'' :: Num a => Int -> a
fibonacci'' n = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in fibs !! n

{- fib map 2 -}