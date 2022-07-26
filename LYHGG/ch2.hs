doubleSmallNumber x = if x > 100
                then x
                else x*2

a = [5, 4, 3, 2, 1]

-- head := takes a list and returns its head
-- head a = 5

-- tail := takes a list and returns chops its head
-- tail a = [4,3,2,1]

-- last := takes a list and returns its last element
-- last a = 1

-- init := takes a list and returns everything execpets its last element
-- init a = [5,4,3,2,1]

-- length := retorna o tamanho da lista
-- null := retorna se a lista está vazia
-- reverese := retorna a lista invertida
-- take n li := extrai n elementos do início da lista li
-- drop n li := descarta n elementos do início da lista li
-- maximum := retorna o maior elemento da lista
-- minimum := retorna o menor elemento da lista
-- sum := retorna a soma dos elementos de uma lista
-- product := retorna o produtório dos elementos de uma lista
-- x `elem` li := retorna se o elemento x está presente na lista li
-- ===========================================================================================================================================
-- Ranges
lia :: [Int]
lia = [1, 3 .. 21] -- cria uma lista de 1 até 21 com pace 2(3-1)
lib :: [Int]
lib = [2, 3 .. ] -- cria uma lista infinita de 2 até INF com pace 1
-- take N lib := mostra os N primeiros elementos da lista lib
-- cycle liP := cycla os elementos da lista liP de forma "infinita"
-- repeat k := cria uma lista infinita apenas de elementos com valor k
length' xs = sum [1 | _ <- xs]

-- Exercicio: pegar uma lista de triangulos cujo cada lado mede até 10 cujo perimetro é igual a 24

especifiedTriangles :: [(Int,Int,Int)]
especifiedTriangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a + b + c == 24, a + b > c, a + c > b, b + c > a]