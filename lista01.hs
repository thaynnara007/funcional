{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor a b = ((not a) && b) || (a && (not b))  
impl a b = (not a) || b
equiv a b = (impl a b) && (impl b a)

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x 1 = x
pow x y 
    | y > 0 = x * pow x (y-1)
	| otherwise = 1/(pow x (-y))
{-
    versao 2 da funçao potencia, exponenciacao rapida   
-}
--pow2 :: Int -> Int -> Int
pow2 a 1 = a
pow2 a b | (b `mod` 2 == 0) = pow2 (a * a) (div b 2)
         | otherwise = a * pow2 a (b - 1)

{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 1 = 1
fatorial x = x * fatorial (x-1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
prime _ 0 = False
prime _ 1 = True
prime x n | (x `mod` n == 0) = False
          | otherwise = prime x (n - 1)
isPrime x = prime x (x-1)

-- Crivo de Erastotenes



{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib n | (n <= 2) = 1
      | otherwise = fib(n - 1) + fib(n - 2)


{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc x y | ( x `mod` y == 0) = y
        | otherwise = mdc y (x `mod` y)

{-
- Calcula um MMC de dois numeros. 
-}
mmc x y = x * (div y (mdc x y))

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y = if (mdc x y == 1) then True else False

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}

conjutura x n |  (isPrime n) && (isPrime (x - n)) = [n, (x-n)]
              | otherwise = conjutura x (n-1)
goldbach x = conjutura x (x-1)
