{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor True False = True
xor False True = True
xor _ _ = False
impl False _ = True
impl _ True = True
impl _ _ = False
equiv a b = (impl a b) && (impl b a)

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x 0 = 1
pow x 1 = x
pow x y | y < 0 = pow (1 / x) y
        | otherwise = pow x (y - 1) * x


{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 0 = 0
fatorial 1 = 1
fatorial x = fatorial (x - 1) * x 

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
isPrime x | x < 0 = False
          | x <= 3 = True
          | x `mod` 2 == 0 = False
          | otherwise = isPrime' x [3..(x - 1)]

isPrime' _ [] = True
isPrime' x (y:ys) | x `mod` y == 0 = False
                  | otherwise = isPrime' x ys

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib 1 = 0
fib 2 = 1
fib x = (fib (x - 1)) + (fib (x - 2))

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc 0 y = y
mdc x 0 = x
mdc x y = mdc y r
    where r = x `mod` y
{-
- Calcula um MMC de dois numeros. 
-}
mmc x y = x * y `div` (mdc x y)

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y = (mdc x y) == 1

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = [(y, z) | y <- filter isPrime [1..x - 1], z <- filter isPrime [1..x - y], y + z == x]
