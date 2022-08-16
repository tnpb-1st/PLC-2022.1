-- Funcao que remove todos os caracteres de uma string que não são maiusculos
removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Tipos mais comuns de typeclasses
-- Eq,Ord,Show,Read, Enum, Bounded, Num, Integral, Floating
