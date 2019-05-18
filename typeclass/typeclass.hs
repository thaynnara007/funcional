{-
Se um tipo é parte de uma typeclass, quer dizer que ela suporta e implementa o comportamento 
especificado pela classe de tipo.
Algumas typeclasses:
+ Eq é usado por tipos que suportam teste por igualdade.
+ Ord é para tipos que têm ordem.
+ Ordering é uma typeclass que pode ser GT, LT ou EQ, significando maior que, menor que e igual a, 
respectivamente.
+ Membros do Show podem ser representados como strings.
+ Read é tipo uma oposição da typeclass Show. A função read recebe uma string e retorna 
um tipo membro de Read.
+ type annotations servem para dizer qual tipo que você quer que uma expressão assuma. 
Fazemos isso adicionando :: no fim da expressão com o tipo desejado.
+ Os membros de Enum são tipos que possuem uma seqüência.
+ Bounded são os tipos que possuem limites - máximo e mínimo.
+ Num é uma typeclass numérica geral.
+ Integral suporta apenas inteiros. Essa typeclass é composta por Int e Integer.
+ Floating inclui apenas números de ponto flutuante, então são Float e Double.
-}

data TrafficLight = Red | Yellow | Green

-- instance é para se fazer novas instâncias de tipos a partir de typeclasses.
instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Read light"
  show Yellow = "Yellow light"
  show Green = "Green light"

-- criando suas proprias typeclass
class Equal a where
  isEqual :: a -> a -> Bool

instance Equal TrafficLight where
  isEqual Red Red = True
  isEqual Green Green = True
  isEqual Yellow Yellow = True
  isEqual _ _ = False

--  typeclasses que são subclasses de outras typeclasses
data List a = Nil | Cons a (List a)

instance (Eq a) => Eq(List a) where
  Nil == Nil = True
  (Cons x xs) == (Cons y ys) = (x == y) && (xs == ys)
  _==_ = False

class (Eq a, Show a) => Num a where
  (+), (-), (*) :: a -> a -> a 
  negate :: a -> a
  abs :: a -> a
  fromInteger :: Integer -> a

class YesNo a where 
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _ ) = True
  yesno Nothing = False

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

myIf :: (YesNo y) => y -> a -> a -> a
myIf yesNoValue yesResult noResult = if yesno yesNoValue then yesResult else noResult

{-
o f não é um tipo concreto (um tipo que um valor pode ter, como Int, Bool ou Maybe String), 
mas sim um construtor de tipos que pegue um parâmetro de tipo, como Maybe Int é um tipo concreto, 
mas Maybe é um construtor de tipo que pega um tipo como parâmetro. O fmap pega uma função de um tipo 
para outro e um functor aplicado em um tipo e retorna um functor aplicado ao outro tipo.
-}

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Main.Functor [] where
  fmap = map

instance Main.Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing

instance Main.Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node a left right) = Node (f a) (Main.fmap f left) (Main.fmap f right)