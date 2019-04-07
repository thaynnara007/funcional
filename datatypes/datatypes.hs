-- Criando um tipo de dado, onde antes da igualdade é o nome e os parâmetros, e depois
-- da igualdade, o contrutor do novo tipo criado
data Pair a b = Pair a b deriving(Show)

-- criando um novo tipo de dado com mais de um construtor
data Point = Point Float Float deriving(Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Duas funções que retornam o primeiro e segundo elemento de um Point
pairFst (Pair x y) = x
pairSnd (Pair x y) = y

-- Uma função que recebe uma forma geométrica e que retorna a sua superfície.
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1 ) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1) 

{-
Write a data type declaration for Triple, a type which contains three elements, 
all of different types. Write functions tripleFst, tripleSnd and tripleTrd to extract 
respectively the first,second and third elements.
-}

data Triple a b c = Triple a b c deriving(Eq, Show)
tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

{-
Write a datatype Quadruple which holds four elements. However, the first two elements must be 
the same type and the last two elements must be the same type. Write a function firstTwo which
returns a list containing the first two elements and a function lastTwo which returns a list 
containing the last two elements.
-}

data Quadruple a b = Quadruple a a b b deriving(Eq, Show)
firstTwo (Quadruple a b c d) = [a,b]
secondTwo (Quadruple a b c d) = [c,d]