module MultisetMap (
  insert_bag,
  remove_bag,
  search_bag,
  union_bag,
  intersection_bag,
  minus,
  inclusion,
  sum_bag,
  size_bag
)
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.Map, onde 
 - cada elemento da lista consiste do dado em si mapeado para sua quantidade. 
 - Eh recomendavel que voce consulte a documentacao de Data.Map
 -}
import Data.Map as Map
import Data.Char

from_just (Just a) = a

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert_bag elem bag 
  | amount == Nothing = insert elem 1 bag
  | otherwise = insert elem ((from_just amount) + 1) bag
  where
    amount = Map.lookup elem bag

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove_bag elem bag 
    | a == Nothing = error "Esse elemento não esta na bag"
    | amount == 0 = Map.delete elem bag 
    | otherwise = insert elem amount bag
    where
      a = Map.lookup elem bag
      amount = (from_just a) - 1

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search_bag elem bag = Map.lookup elem bag

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}

union_bag bag1 bag2 = Map.unionWith (max) bag1 bag2

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection_bag bag1 bag2 = Map.intersectionWith (min) bag1 bag2

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}

f al ar = if (al-ar) > 0 then Just (al-ar) else Nothing      
minus bag1 bag2 = Map.differenceWith f bag1 bag2

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion bag1 bag2 = Map.isSubmapOfBy (<=) bag1 bag2

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
sum_bag bag1 bag2 = Map.unionWith (+) bag1 bag2

{-
 - Retorna a quantidade total de elementos no Bag
-}
size_bag bag = Map.size bag