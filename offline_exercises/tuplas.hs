{- 1. Defina a função menorMaior que recebe três inteiros e retorna uma tupla com o menor
e o maior deles, respectivamente. -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
menorMaior :: Integer -> Integer -> Integer -> (Integer, Integer)
menorMaior x y z = (greater x y z, lower x y z)
    where
        greater x y z
          | x >= y && x >= z = x
          | y >= x && y >= z = y
          | otherwise = z
        lower x y z
          | x <= y && x <= z = x
          | y <= x && y <= z = y
          | otherwise = z
{- 2. Defina a função ordenaTripla que recebe uma tripla de inteiros e ordena a mesma. -}
ordenaTripla :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
ordenaTripla (x, y, z) = (lw (lw x y) z, x + y + z - gt (gt x y) z - lw (lw x y) z, gt (gt x y) z)
    where
        gt x y = if x >= y then x else y
        lw x y = if x <= y then x else y
{- 3. Uma linha pode ser representada da seguinte forma
• Defina funções para
– retornar a primeira coordenada de um ponto
– retornar a segunda coordenada de um ponto
– indicar se uma reta é vertical ou não ( x1 = x2) -}
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

_fst :: Ponto -> Float
_fst (x,_) = x

_snd :: Ponto -> Float
_snd (_,y) = y;

retaEhVertical :: Reta -> Bool
retaEhVertical r
    | (_fst (fst r) == _fst (snd r) ) && (_snd (fst r) /= _snd (snd r)) = True
    | otherwise = False

{- 4. Se uma reta é dada por (y − y1)/(x − x1) = (y2 − y1)/(x2 − x1),
defina uma função pontoY :: Float −> Reta −> Float que, dada uma coordenada x e
uma reta, retorne a coordenada y, tal que o ponto (x, y) faça parte da reta. -}
coordY :: Float -> Reta -> Float
coordY x r = m * x + b
    where
        m = snd (snd r) - snd (fst r)/ (fst (snd r) - fst (fst r))
        b = snd (snd r) - (fst (snd r) * m)