--
data Estacao = Inverno | Primavera | Verao | Outono
data Temperatura = Quente | Frio
    deriving(Show)

clima :: Estacao -> Temperatura
clima Inverno = Frio
clima _ = Quente

--
data DiasSemana = Seg | Ter | Qua | Qui | Sex | Sab
    deriving (Eq, Ord, Enum, Show, Read)

--
type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade

showPessoa :: Pessoas -> String
showPessoa (Pessoa n a) = n ++ "---" ++ show a

-- 
data Shape = Circle Float
            | Rectangle Float Float
            deriving (Show)

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False
isRound _ = False

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle b h) = b * h