{-
- Dada uma tupla, divide o primeiro pelo segundo usando pattern matching.
- Ela deve ser indefinida quando o denominador for zero.
-}
divTuple (_, 0) = undefined
divTuple (x, y) = x/y

{-
 - Calcula o somatorio entre dois numeros a e b (a < b). Procure usar alguma funcao pronta sobre listas. 
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}
somatorio a b = sum[a..b] 

{-
 - Calcula o somatorio (recursivo) entre dois numeros a e b (a < b).
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}
somatorioRec _ 0 = 0
somatorioRec a b = a + (somatorioRec (a+1) (b-1))

sumRec a b
    | a == b = b
    | otherwise = a + (sumRec (a+1) b)

-- Defina a funcao que eleva um membro ao quadrado
square x = x * x 

-- Soma os quadrados de dois numeros.
sumSquares x y = square x + square y

-- Defina uma funcao de alta ordem que aceita uma função (Int -> Int) e aplica a funcao a dois numeros
higherOrderSum f a b = f a b


--Implemente a funcao mapFilter que primeiro aplica o map de uma funcao f a uma lista e depois aplica a funcao filter
-- a lista resultante. Procure usar a composicao de funcoes
mapFilter f p xs = filter p (map f xs)