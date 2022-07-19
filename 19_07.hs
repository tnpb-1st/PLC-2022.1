-- exemplo de uma função do 2o grau

umaRaiz :: Float -> Float -> Float -> Float
umaRaiz a b c = -b / (2.0 * a)

duasRaizes :: Float -> Float -> Float -> (Float, Float)
duasRaizes a b c = (d-e, d+e)
    where
        d = -b/(2.0*a)
        e = sqrt((b*b) - 4 * a * c)/(2.0*a)

raizes :: Float -> Float -> Float -> String
raizes a b c
    | 4.0 * a * c == 0.0 = show (umaRaiz a b c)
    | 4.0 * a * c > 0.0  = (show r1) ++ " " ++ (show r2)
    | otherwise = show("Nao existem solucoes reais\n")
        where
            p = duasRaizes a b c
            r1 = fst p
            r2 = snd p

-- sinônimos, funciona como o typedef do c++
-- type String = [Char]
type Nome = String
type Idade = Int
type NumTel = String
type Pessoa = (Nome, Idade, NumTel)

nome :: Pessoa -> Nome
nome ( n, i, nT) = n

nomeIdade :: Pessoa -> String
nomeIdade (n, i, nT) = n ++ " tem " ++ show(i) ++ " anos de idade"

-- Listas
-- [1 .. 10] == range(1,11)
-- [1 .. 7.0]
-- [1 .. 7.4]
-- [1 .. 7.5]
-- [1 , 3 .. 10]

-- Compressao de listas
-- {x ** 2 | x e N}
-- [expressao | x <- lista geradora]

-- [2 * x | x <- [1 .. 4]]
-- [x ** 3 | x <- [1 .. 4] y <- [15..20]]
-- [(x,y) | x <- [1 .. 4], y <- [15 .. 20]]

-- [expressao | x <- lista geradora, filtro]
-- [x | x <- [1 .. 20], x `mod` 2 == 0 && x > 15]

ehPar :: Int -> Bool
ehPar x = mod x 2 == 0

todosPares :: [Int] -> [Int]
todosPares l = [x | x <- l , ehPar x]

tamLista :: [Int] -> [Int]
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs