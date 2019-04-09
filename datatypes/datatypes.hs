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

{-
uma função que move uma figura. Recebe uma figura, o tanto a se mover no eixo x e no eixo y 
para retornar uma nova figura de mesmas dimensões, localizada em outra posição.
-}

nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b)) 

-- Tipos paramêtricos
just = Prelude.Just
nothing = Prelude.Nothing

data Maybe a = Nothing | Just a


firstElement :: [a] -> Prelude.Maybe a
firstElement [] = nothing
firstElement (x:xs) = just x

findElement p [] = nothing
findElement p (x:xs) = if p x then just x else findElement p xs


{-
Write a datatype Tuple which can hold one, two, three or four elements, depending on the constructor 
(that is, there should be four constructors, one for each number of arguments). Also provide
functions tuple1 through tuple4 which take a tuple and return Just the value in that position, 
or Nothing if the number is invalid (i.e., you ask for the tuple4 on a tuple holding only two elements).
-}

data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Eq, Show)

tuple1 (Tuple1 a) =  just a
tuple1 (Tuple2 a b) = just a
tuple1 (Tuple3 a b c) = just a
tuple1 (Tuple4 a b c d) = just a

tuple2 (Tuple2 a b) = just b  
tuple2 (Tuple3 a b c) = just b
tuple2 (Tuple4 a b c d) = just b 
tuple2 _ = nothing 

tuple3 (Tuple3 a b c) = just c
tuple3 (Tuple4 a b c d) = just c 
tuple3 _ = nothing

tuple4 (Tuple4 a b c d) = just d
tuple4 _ = nothing

-- Estrutura de dados recursivas
{-
Onde o valor de um tipo contém valores daquele tipo, que por sua vez contém mais 
valores do mesmo tipo, e assim por diante.
[5]. Isso é apenas açúcar sintático para 5:[]. Do lado esquerdo do :, há um valor e do lado direito, 
há uma lista. E neste caso, há uma lista vazia. E agora sobre a lista [4,5]? 
Bem, isso é desaçucarado para 4:(5:[]). 
-}

data MyList a = Nil | Cons a (MyList a) deriving (Eq, Show)
-- ou
infixr 5 :-
data List a = Empty | a :- List a deriving (Eq, Show)

-- Criando uma função que junta duas das nossas listas em uma. 
-- Assim é como ++ é definida para listas normais: 

infixr 5 -+-
(-+-) ::List a -> List a -> List a
Empty -+- ys = ys 
(x:-xs) -+- ys = x :- (xs -+- ys)

{-
Write functions listHead, listTail, listFoldl and listFoldr which are equivalent to their 
Prelude twins, but function on our List datatype.
-}

listHead Empty = error "Empty list"
listHead (x:-xs) = x

listTail Empty = error "Empty list"
listTail (x:-xs) = xs

listFoldr p v Empty = v
listFoldr p v (x:-xs) = p x (listFoldr p v xs)

listFoldl p v Empty = v
listFoldl p v (x:-xs) = listFoldl p (p v x) xs 

-- árvore de busca binária. 
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq)
-- constroi um no sem arvores a esquerna e direita
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree
  