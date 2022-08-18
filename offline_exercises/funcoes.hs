{- 1. Definir a função dobro de tipo Integer−>Integer. A função deve receber um argumento
e devolvê-lo multiplicado por dois. -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import System.Win32 (COORD(y))
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
dobro :: (Num a) => a -> a
dobro x = 2 * x
{- 2. Definir a função quadruplo que utiliza a função dobro do exercício anterior para devolver
o seu argumento multiplicado por quatro -}
quadruplo :: (Num a) => a -> a
quadruplo x = dobro (dobro x)
{- 3. Definir a função poli2. Devem ser necessários quatro argumentos do tipo Double ( a,
b, c, e x) e devolver a ∗ x²+ b ∗ x + c. Definir a assinatura do tipo poli2 , ou seja,
poli2 :: alguma coisa -}
poli2 :: Double -> Double -> Double -> Double -> Double
poli2 a b c x = a * (x*x) + b * x + c

showPoli2 :: Double -> Double -> Double -> Double -> IO()
showPoli2 a b c x = putStrLn (term1 ++ " + " ++ term2 ++ " + " ++ term3)
    where
        term1 = show a ++ "X²"
        term2 = show b ++ "X"
        term3 = show c

{- 4. Definir a função parImpar que devolve "par" (string) para entradas pares e "impar"
(string) para entradas ímpares. Defina uma função para determinar se um número é
par. -}
parImpar :: (Integral a) => a -> String
parImpar x
    | ehPar x = "par"
    | otherwise = "impar"
    where
        ehPar x = x `mod` 2 == 1
{- 5. Defina a função maxFour :: Integer −> Integer −> Integer −> Integer −> Integer que
retorna o máximo de quatro inteiros Dê três definições dessa função: a primeira descrita
com base em maxThree; a segunda deve usar a função max e a terceira deve usar as
funções max e maxThree. -}
maxThree :: (Ord a) => a -> a -> a -> a
maxThree a b c
  | a >= b && (a >= c) = a
  | (b >= c) && (b >= a) = b
  | otherwise = c

maxFour :: (Ord a) => a -> a -> a -> a -> a
maxFour a b c d
    | (a >= b) && (a >= c) && (a >= d) = a
    | (b >= a) && (b >= c) && (b >= d) = b
    | (c >= a) && (c >= b) && (c >= d) = c
    | otherwise = d

maxFour' :: (Ord a) => a -> a -> a -> a -> a
maxFour' a b c d = if k >= d then k else d
    where
        k = maxThree a b c

maxFour'' :: (Ord a) => a -> a -> a -> a -> a
maxFour'' a b c d = max (max (max a b) c) d

maxFour''' :: (Ord a) => a -> a -> a -> a -> a
maxFour''' a b c d = max (maxThree a b c) d
{- 6. Defina a função quantosIguais :: Integer −> Integer −> Integer −> Integer que re-
torna quantos dos três argumentos são iguais. Por exemplo,

quantosIguais 56 32 12 = 0
quantosIguais 12 12 43 = 2 -}
quantosIguais :: (Eq a, Num a) => a -> a -> a -> Integer
quantosIguais a b c
    | (eq a b) && (eq a c) = 3
    | (eq a b) || (eq a c) || (eq b c) = 2
    | otherwise = 1
    where
        eq :: (Eq a) => a -> a -> Bool
        eq a b = a == b

{- 7. Usando casamento de padrão, definir a função ehZero que retorna verdadeiro se for
dado como argumento um inteiro que seja 0, e falso, caso contrário. Definir o tipo da
função ehZero -}
ehZero :: (Integral a) => a -> Bool
ehZero 0 = True
ehZero x = False
{- 8. Usando recursão, implemente a função sumTo, de modo que sumTo n calcula o valor de
1 + 2 + ...+ n -}
sumTo :: Integer -> Integer
sumTo 1 = 1
sumTo x = x + sumTo (x-1)
{- 9. Defina a função potencia, de modo que potencia n k calcula n elevado a k. Use recursão. -}
fastExpo :: Integer -> Integer -> Integer
fastExpo 0 _ = 0
fastExpo _ 0 = 1
fastExpo x y
    | (y `mod` 2 == 1) = x * fastExpo x (y `div` 2) * fastExpo x (y `div` 2)
    | otherwise = fastExpo x (y `div` 2) * fastExpo x (y `div` 2)
{- 10. Usando recursão, compute os coeficientes binomiais dados pelas seguintes equações
B(n, k) = B(n − 1, k) + B(n − 1, k − 1)
B(n, 0) = 1
B(0, k) = 0, quando k > 0
Dica: usar casamento de padrão pode ser de grande ajuda. -}
binCoef :: Integer -> Integer -> Integer
binCoef _ 0 = 1
binCoef 0 k = if k > 0 then 0 else error "k deve ser maior que 0"
binCoef n k = binCoef (n-1) k + binCoef (n-1) (k-1)
{- 11. Os números de Tribonacci são dados pelas seguintes equações
T(1) = 1
T(2) = 1
T(3) = 2
T(n + 1) = T(n) + T(n − 1) + T(n − 2)

Implemente uma função recursiva eficiente que calcula T n. Considere o uso de uma
função auxiliar. -}
tribonacci :: Int -> Integer
tribonacci = (map trib [0 ..] !!)
    where
        trib 1 = 1
        trib 2 = 1
        trib 3 = 2
        trib n = tribonacci n + tribonacci n-1 + tribonacci n-2

{- 12. Defina a função addEspacos que produz um string com uma quantidade n de espaços.
addEspacos :: Int −> String -}

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos x = " " ++ addEspacos(x-1)

{- 13. Defina a função paraDireita utilizando a definição de addEspacos para adiciconar uma
quantidade n de espaços à esquerda de um dado String, movendo o mesmo para a direita.
paraDireita :: Int −> String −> String -}
paraDireita :: Int -> String -> String
paraDireita x str = addEspacos (x) ++ str