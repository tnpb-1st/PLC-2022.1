-- Pattern matching in list comprehension
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal pattern" #-}

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: Integer -> Integer
factorial 1 = 1
factorial x = x * factorial(x-1)

-- Add pairs in a list
xs :: [(Integer,Integer)]
xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
xb = [(a+c,b+d) | (a,b) <- xs,(c,d) <- xs]

-- Get the head of a list
head' :: [a] -> a
head' [] = error "Can't call head on empty list!"
head' (x:_) = x


-- Extracting elements from a tuple
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- Informations about the list
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- reverse function
rev :: [a] -> [a]
rev [] = []
rev (h:tail) = rev tail ++ [h]

-- length function
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- fibonacci function
fib :: (Integral a) => a -> Integer
fib 0 = 0
fib 1 = 1
fib x =  fib(x-1) + fib(x-2)

people :: [(Float, Float)]
people = [(60, 1.73), (94, 1.84), (118, 1.95), (79, 1.6), (60, 1.99), (84, 1.99), (118, 1.61), (76, 1.6), (75, 2.08), (70, 1.97)]

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

-- Guards
imcCalc :: (RealFloat a) => a -> a -> String
imcCalc peso altura
    | imc < 18.5 = "Peso abaixo do Normal"
    | imc <= 24.9 = "Saudavel"
    | imc < 30 = "Sobrepeso"
    | imc <= 30 = "Obesidade"
    | otherwise = "Obesidade Extrema!"
    where imc = peso / (altura * altura)

calcImcs :: (RealFloat a) => [(a,a)] -> [a]
calcImcs xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / (height * height)

-- Using Let

listOfNumbers, listOfSquares :: [Integer]
listOfNumbers = [10 .. 20]
listOfSquares = [let square x = x * x in square num | num <- listOfNumbers]

calcImcs' :: (RealFloat a) => [(a,a)] -> [a]
calcImcs' imcs = [imc | (p, alt) <- imcs, let imc = p / (alt * alt)]

-- Case expressions
_head :: [a] -> a
_head xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

-- case é uma expressão
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list"
-- where é um açucar sintatico para case expressions em funções
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."