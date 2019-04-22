-- QUESTAO 01
sequencia 2 = [1,1]
sequencia x = fib ++ [(last fib) + (last (init fib))]
    where 
        fib = sequencia (x - 1)

fib 1 = 1
fib 2 = 1
fib x = last (sequencia x)

-- QUESTAO 02
get_repetidos [] _ = []
get_repetidos [x] _ = []
get_repetidos (x:xs) k 
        | length ocorrencias >= k = x:(get_repetidos lista_sem_x k)
        | otherwise = get_repetidos lista_sem_x k
        where
            ocorrencias = x:(filter (==x) xs)
            lista_sem_x = filter (/=x) xs

-- QUESTAO 03
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)
-- let bst = Node 5 (Node 3 (Node 1 NIL NIL) (Node 4 NIL NIL)) (Node 7 (Node 6 NIL NIL) (Node 8 NIL NIL))
search_bst e NIL = False
search_bst e (Node a left rigth)
		| e < a = search_bst e left
		| e > a = search_bst e rigth
    | otherwise = True


my_deep e NIL = 0
my_deep e (Node a left rigth)
    | e < a = 1 + (my_deep e left)
    | e > a = 1 + (my_deep e rigth)
    | otherwise = 0

deep e bst = if (search_bst e bst) then my_deep e bst else -1

leaves NIL = []
leaves (Node a NIL NIL) = [a]
leaves (Node a left right) = (leaves left) ++ (leaves right)

mirror (Node a NIL NIL) = Node a NIL NIL
mirror (Node a left NIL) = Node a NIL (mirror left)
mirror (Node a NIL right) = Node a (mirror right) NIL
mirror (Node a left right) = Node a new_left new_right
    where
      new_left = mirror right
      new_right = mirror left

mapTree f (Node a NIL NIL) = Node (f a) NIL NIL
mapTree f (Node a left NIL) = Node (f a) (mapTree f left) NIL
mapTree f (Node a NIL right) = Node (f a) NIL (mapTree f right)
mapTree f (Node a left right) = Node (f a) (mapTree f left) (mapTree f right)
          
