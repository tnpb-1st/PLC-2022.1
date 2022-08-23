{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isAsciiLower" #-}
import Data.Char
import Data.Typeable
{- 1. Defina a função
paraMaiuscula :: String −> String
que transforma toda letra minúscula em uma string em maiúscula. Use compreensão de
listas.
Modifique esta função para se comportar da mesma forma, mas removendo todos os
caracteres que não são letras da string resultante. -}
upperString :: String -> String
upperString [] = []
upperString (x:xs)
    | (x >= 'a') && (x <= 'z') = toUpper x : upperString xs
    | otherwise = x : upperString xs

remCharacters :: String -> String
remCharacters [] = []
remCharacters (x:xs)
    | isAsciiLower x || isAsciiUpper x = x : remCharacters xs
    | otherwise = remCharacters xs

{- 2. Defina a função
divisores :: Integer −> [Integer]
que retorna uma lista de divisores e um inteiro positivo e uma lista vazia para outras
entradas -}
divisores :: Integer -> [Integer]
divisores n
    | n <= 0 = []
    | otherwise = [x | x <- [1 .. n], n `mod` x == 0]

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x
    | x < 0 = isPrime(x * (-1))
    | otherwise = head xs == 1 && last xs == x && length xs == 2
    where
        xs = divisores x

{- 3. Dada uma lista de inteiros, defina a função menorLista que encontra o menor inteiro
dessa lista -}
menorLista :: [Int] -> Int
menorLista [] = maxBound :: Int
menorLista [a] = a
menorLista (x:xs)
    | x <= menorLista xs = x
    | otherwise = menorLista xs

{- 4. Defina a função
fibTable :: Integer −> String
que produz uma tabela com os números de Fibonacci. A saída para fibTable 6 é -}

fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (x,y) = (y, x+y)

fibPair :: Integer -> (Integer, Integer)
fibPair 0 = (0, 1)
fibPair n = fibStep(fibPair(n-1))

fastFib :: Integer -> Integer
fastFib n = fst (fibPair n)

fibTable :: Integer -> String
fibTable 2 = "n  fib n\n" ++ "0  0\n"
fibTable n = fibTable(n-1) ++ show (n-2) ++ "  " ++ show (fastFib (n-2)) ++ "\n"