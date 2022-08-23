{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Use null" #-}

-- Tuplas, definição
parInt :: (Int, Int)
parInt = (22,33)

minAndMax :: Integer -> Integer -> (Integer, Integer)
minAndMax x y
    | x >= y = (y,x)
    | otherwise = (x,y)

addPair :: (Integer, Integer) -> Integer
addPair (x,y) = x + y

shift :: ((Integer, Integer), Integer) -> (Integer, (Integer,Integer))
shift ((x,y), z) = (x, (y,z))

addPair2 :: (Integer, Integer) -> Integer
addPair2 p = fst p + snd p

-- Tuplas, exemplos
{- Fibonacci rápido com "pairs" -}
fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (u,v) = (v, u+v)

fibPair :: Integer -> (Integer, Integer)
fibPair n
    | n == 0 = (0,1)
    | otherwise = fibStep (fibPair(n-1))

fastFib :: Integer -> Integer
fastFib n = fst (fibPair n)

umaRaiz :: Float -> Float -> Float -> Float
umaRaiz a b c = -b / (2*a)

duasRaizes :: Float -> Float -> Float -> (Float,Float)
duasRaizes a b c = (-b + sqrt(bhaskara) / (2*a), -b - sqrt(bhaskara) / (2*a))
    where bhaskara = b * b + 4 * a * c

solveEqSnd :: Float -> Float -> Float -> String
solveEqSnd a b c = "A equacao possui " ++ res
    where
        bask = b * b - (4 * a * c)
        duasRaizes = ((-b + sqrt(bask))/(2*a), (-b - sqrt(bask))/(2*a))
        res = case bask of 0 -> "como solucao uma raiz real " ++ show (fst duasRaizes)
                           x -> if x > 0 then "como solucao duas raizes reais " ++ show (fst duasRaizes) ++ " " ++ show (snd duasRaizes) else "nenhuma solucao nos reais"


type Nome = String
type Idade = Int
type Fone = Int
type Pessoa = (Nome, Idade, Fone)

nome :: Pessoa -> Nome
nome (n, i, f) = n

{- Listas introdução -}
lista :: (Num a) => [a]
lista = 1:2:3:[] -- equivalente a [1, 2, 3]
-- pegar elemento de indice X
a :: Integer
a = lista !! 2
-- Função que diz se todos os elementos de uma lista são ímpares
todosImpares :: (Integral a) => [a] -> Bool
todosImpares xs = [] ==  ([x | x <- xs, even x])

-- Função QuickSort
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort lessEqual ++ [x] ++ quickSort greaterThan
    where
        lessEqual = [y | y <- xs, y <= x]
        greaterThan = [y | y <- xs, y > x]