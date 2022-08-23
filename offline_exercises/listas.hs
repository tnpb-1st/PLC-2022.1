{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use foldr" #-}
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

{- 5. Defina função measure que, para um lista vazia, retorna -1 e ,para outras listas, retorna
o tamanho da lista -}

measure :: [t] -> Int
measure [] = -1
measure (x:xs) = 1 + measure xs

{- 6. Defina a função takeFinal que retorna os últimos n elementos de uma lista dada como
argumento -}

takeFinal :: Int -> [t] -> [t]
takeFinal _ [] = []
takeFinal n l
    | n >= length l = l
    | otherwise = take n (reverse l)

{- 7. Defina uma função que remove o enésimo elemento de uma lista, ou seja, retorna uma
lista que idêntica à lista recebida como argumento, com exceção de que o enésimo não
consta na lista de retorno. A indexação começa em zero. -}

remove :: Int -> [t] -> [t]
remove _ [] = []
remove 0 (x:xs) = xs
remove n (x:xs) = x : remove (n-1) xs

{- 8. Dê uma definição com casamento de padrão de uma função que retorna o primeiro inteiro,
se houver, em uma lista incrementado de um, ou retorna zero, do contrário. Apresente
também uma solução sem casamento de padrão, mas usando funções da biblioteca. -}
firstInt :: (Integral a) => [a] -> a
firstInt [] = 0
firstInt (x:xs) = x + 1

firstInt' :: (Integral a) => [a] -> a
firstInt' l = if null l then 0 else 1 + head l

{- 9. Dê uma definição com casamento de padrão de uma função que faz a adição dos dois
primeiros elementos de uma lista, se a lista contém pelo menos dois elementos; retorna
a cabeça da lista, se houver um elemento apenas; retorna zero, do contrário. Apresente
também uma solução sem casamento de padrão, mas usando funções da biblioteca. -}

sumTwo :: (Integral a) => [a] -> a
sumTwo [] = 0
sumTwo [a] = a
sumTwo (x:xs) = x + head xs

sumTwo' :: (Integral a) => [a] -> a
sumTwo' xs
    | null xs = 0
    | length xs == 1 = head xs
    | otherwise = head xs + xs !! 1

{- 10. Defina a função -}
produto :: [Integer] -> Integer
produto [] = 1
produto (x:xs) = x * produto xs

{- 11. Defina a função -}

countElem :: Integer -> [Integer] -> Int
countElem _ [] = 0
countElem n (x:xs)
    | n == x = 1 + countElem n xs
    | otherwise = countElem n xs

unique :: [Integer] -> [Integer]
unique [] = []
unique l = [x | x <- l, countElem x l == 1]
 
{- 12. Defina uma função que verifica se uma lista dada como argumento está em ordem cres-
cente. Use recursão e casamento de padrão. Não use funções de bibliotecas -}

ehCrescente :: [Integer] -> Bool
ehCrescente [] = True
ehCrescente (x : xs)
    | null xs = True
    | x > head xs = False
    | otherwise = ehCrescente xs

