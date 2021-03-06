module MultisetList (
  Bag(Bag),
  insert_bag,
  remove_bag,
  search,
  union_bags,
  intersection_bags,
  minus_bags,
  inclusion,
  sum_bags,
  size
)
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde 
 - cada elemento da lista consiste do dado em si e sua quantidade (um par). 
 - Eh recomendavel que voce consulte a documentacao de Data.List
 -}
import Data.List as List

data Bag list1 list2 = Nil | Bag list1 list2 deriving (Eq,Show)

from_just (Just a) = a

get_nums (Bag list1 list2) = list1
get_amount (Bag list1 list2) = list2

insert_num elem [] = [elem]
insert_num elem (x:xs) = x:(insert_num elem xs)

change_amount 0 amount (x:xs) = (x+amount):xs
change_amount index amount (x:xs) = x:(change_amount (index-1) amount xs) 
{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert_bag elem (Bag list1 list2)
  | index == Nothing = Bag newList1 newList2
  | otherwise = Bag list1 (change_amount (from_just index) 1 list2)
  where
    index = elemIndex elem list1
    newList1 = insert_num elem list1
    newList2 = insert_num 1 list2

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove_num elem (x:xs)
    | elem == x = xs
    | otherwise = x:(remove_num elem xs)

remove_amount 0 (x:xs) = xs
remove_amount index (x:xs) = x:(remove_amount (index-1) xs)

remove_bag elem (Bag list1 list2) 
  | index_num == Nothing = error "Esse elemento não existe na bag"
  | amount == 1 = Bag new_list1 new_list2
  | otherwise = Bag list1 (change_amount i (-1) list2)
  where
    index_num = elemIndex elem list1
    i = from_just index_num
    amount = list2 !! i
    new_list1 = remove_num elem list1
    new_list2 = remove_amount i list2
  
{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search elem (Bag list1 list2) 
    | index == Nothing = error "Esse elemento não existe na bag"
    | otherwise = list2 !! i
    where
      index = elemIndex elem list1
      i = from_just index

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
union_amount [] (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2) = []
union_amount (x:xs) (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2)
      | index_b1 == Nothing = amount_b2:(union_amount xs (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2))
      | index_b2 == Nothing = amount_b1:(union_amount xs (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2))
      | otherwise = (max amount_b1 amount_b2):(union_amount xs (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2))
      where
        index_b1 = elemIndex x list1_b1
        index_b2 = elemIndex x list1_b2
        amount_b1 = list2_b1 !! (from_just index_b1)
        amount_b2 = list2_b2 !! (from_just index_b2)

union_bags (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2) = Bag new_list1 new_list2
        where
          new_list1 = list1_b1 `union` list1_b2
          new_list2 = union_amount new_list1 (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2)

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso nenhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection_amount [] (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2) = []
intersection_amount (x:xs) (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2) = (min amount_b1 amount_b2):(intersection_amount xs (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2))
  where
    amount_b1 = search x (Bag list1_b1 list2_b1)
    amount_b2 = search x (Bag list1_b2 list2_b2)

intersection_bags (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2) = Bag new_list1 new_list2
    where
      new_list1 = intersect list1_b1 list1_b2
      new_list2 = intersection_amount new_list1 (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2)

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus_nums [] (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2) = []
minus_nums (x:xs) (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2)
      | index_b2 == Nothing = amount_b1:(minus_nums xs (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2))
      | otherwise = if (result > 0) then x:(minus_nums xs (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2)) else (minus_nums xs (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2))
      where
        index_b2 = elemIndex x list2_b2
        amount_b1 = search x (Bag list1_b1 list2_b1)
        amount_b2 = list2_b2 !! (from_just index_b2)
        result = (amount_b1-amount_b2)
        
minus_amount [] (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2) = []
minus_amount (x:xs) (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2) =
  result:(minus_amount xs (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2)) 
        where
          index_b2 = elemIndex x list2_b2
          amount_b1 = search x (Bag list1_b1 list2_b1)
          amount_b2 = list2_b2 !! (from_just index_b2)
          result = (amount_b1-amount_b2)

minus_bags (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2) = Bag new_list1 new_list2
      where
        new_list1 = minus_nums list1_b1 (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2)
        new_list2 = minus_amount new_list1 (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2)
{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion (Bag [] list2_b1) (Bag list1_b2 list2_b2) = True
inclusion (Bag (x:xs) list2_b1) (Bag list1_b2 list2_b2) 
        | elem x list1_b2 = if (amount_b1 <= amount_b2) then inclusion (Bag xs list2_b1) (Bag list1_b2 list2_b2) else False
        | otherwise = False
        where
          amount_b1 = search x (Bag (x:xs) list2_b1)
          amount_b2 = search x (Bag list1_b2 list2_b2)
{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
sum_amount [] (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2) = []
sum_amount (x:xs) (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2)
      | index_b1 == Nothing = amount_b2:(sum_amount xs (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2))
      | index_b2 == Nothing = amount_b1:(sum_amount xs (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2))
      | otherwise = (amount_b1 + amount_b2):(sum_amount xs (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2))
      where
        index_b1 = elemIndex x list1_b1
        index_b2 = elemIndex x list1_b2
        amount_b1 = list2_b1 !! (from_just index_b1)
        amount_b2 = list2_b2 !! (from_just index_b2)

sum_bags (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2) = Bag new_list1 new_list2
  where
    new_list1 = list1_b1 `union` list1_b2
    new_list2 = sum_amount new_list1 (Bag list1_b1 list2_b1) (Bag list1_b2 list2_b2)
{-
 - Retorna a quantidade total de elementos no Bag
-}
size (Bag list1 list2) = length list1 