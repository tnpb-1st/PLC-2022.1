-- Tipos Algebricos(data)
-- Enumeracao

data Temperatura = Fria | Quente
    deriving (Eq, Show, Ord, Enum)

data Estacao = Verao | Outono | Inverno | Primavera
    deriving (Eq, Show, Ord, Enum)

clima :: Estacao -> Temperatura
clima Inverno = Fria
clima x = Quente
-- 
type Nome = String
type Idade = Int

data Pessoas = Pessoa Nome Idade
                deriving(Show, Eq)

exibePessoa :: Pessoas -> String
exibePessoa (Pessoa str n) = str ++ " tem idade: " ++ show n

--
type NomeProduto = String
type Preco = Int
data Produtos = Produto NomeProduto Preco

type PessoaTupla = (Nome, Idade)
type ProdutosTupla = (NomeProduto, Preco)

showPessoasTupla :: PessoaTupla -> String
showPessoasTupla (n,i) = n ++ " --- " ++ show i

--Tipos recursivos
data Expr = Lit Int
            | Expr + Expr
            | Add Expr Expr
            | Sub Expr Expr

exp1 = Lit 1
exp2 = Add(Lit 1) (Add (Lit 2) Lit(3))
exp3 = Sub(Add (Lit 6) (Lit 8)) ((Add (Sub (Lit 4) (Lit 2))) (Lit 1))
