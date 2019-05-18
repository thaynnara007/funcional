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