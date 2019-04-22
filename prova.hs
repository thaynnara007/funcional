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
