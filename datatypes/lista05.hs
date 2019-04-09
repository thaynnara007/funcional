--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a b c) = a 
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadruple a a b b deriving(Eq, Show)

firstTwo (Quadruple a b c d) = (a,b)
secondTwo (Quadruple a b c d) = (c,d)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = 
    Tuple1 a | 
    Tuple2 a b | 
    Tuple3 a b c | 
    Tuple4 a b c d 
    deriving (Eq, Show)

tuple1 (Tuple1 a) =  Just a
tuple1 (Tuple2 a b) = Just a
tuple1 (Tuple3 a b c) = Just a
tuple1 (Tuple4 a b c d) = Just a

tuple2 (Tuple2 a b) = Just b  
tuple2 (Tuple3 a b c) = Just b
tuple2 (Tuple4 a b c d) = Just b 
tuple2 _ = Nothing 

tuple3 (Tuple3 a b c) = Just c
tuple3 (Tuple4 a b c d) = Just c 
tuple3 _ = Nothing

tuple4 (Tuple4 a b c d) = Just d
tuple4 _ = Nothing 

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

getNode (Node a left right) = a

--verifica se uma BT é uma BST
isBST NIL = True
isBST (Node a NIL NIL) = True
isBST (Node a left NIL)
    | a > (getNode left) = isBST left
    | otherwise = False
isBST (Node a NIL right)
    | a < (getNode right) = isBST right
    | otherwise = False
isBST (Node a left right)
    | ( a > nodeLeft && a < nodeRight) = (isBST left) && (isBST right )
    | otherwise = False
    where
        nodeLeft = getNode left
        nodeRight = getNode right

--insere uma nova chave na BST retornando a BST modificada
singleton :: a -> BinaryTree a
singleton a = Node a NIL NIL

insert elemento NIL = singleton elemento
insert elemento (Node a left right)
    | elemento < a = Node a (insert elemento left) right
    | elemento > a = Node a left (insert elemento right)
    | otherwise = Node a left right

--retorna o Node da BST contendo o dado procurado ou entao NIL
search elemento NIL =  NIL
search elemento (Node a left right)
    | elemento < a = search  elemento left
    | elemento > a = search elemento right
    | otherwise = singleton a

--retorna o elmento maximo da BST
maximo (Node a NIL NIL) = a
maximo (Node a left NIL) = a
maximo (Node a left right) = maximo right

--retorna o elemento minimo da BST
minimo (Node a NIL NIL) = a
minimo (Node a NIL right) = a
minimo (Node a left right) = minimo left

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor = undefined

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor = undefined

--remove ume lemento da BST
remove = undefined

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder = undefined
order = undefined
postOrder = undefined
