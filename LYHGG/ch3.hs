-- Funcao que remove todos os caracteres de uma string que não são maiusculos
removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Tipos mais comuns de typeclasses
-- Eq,Ord,Show,Read, Enum, Bounded, Num, Integral, Floating

{- Funções prontas para listas -}
-- head, tail, last, init, reverse
-- length, null, maximum, minimum, sum, product, elem
-- take, drop

-- cycle -> pega uma lista de input e gera uma lista infinita fazendo ciclos da lista de input

-- l2 :: [char]
-- l2 = take 12 cycle("LOL ")

-- repeat -> análogo ao comando de cycle, porém repete apenas um elemento
-- take 12 (repeat "LOL ")

{- List comprehension com filter -}
xs = [7 .. 13]
-- pega os dobros dos primeiros 10 números naturais
-- l2 = [x*2 | x <- [1 .. 10]]
-- pega os dobros dos primeiros 10 elementos em que seus dobros são >= 12
-- l2 = [x*2 | x <- [1 .. 10], x*2 >= 12]
-- numeros impares e menores que 10 são substituidos por "BOOM!" c.c. por "BANG!"
l2 = [if x < 10 then "BOOM" else "BANG!" | x <- xs, odd x]
-- todos os produtos entre duas listas com valor maior que 50
l3 = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
-- possível função para calcular length
-- ela troca todos os elementos por 1 e vai somando
length' xs = sum [1 | _ <- xs]   
-- remover todos os elementos ímpares de uma lista de listas
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
l4 = [ [ x | x <- xs, even x ] | xs <- xxs] 
