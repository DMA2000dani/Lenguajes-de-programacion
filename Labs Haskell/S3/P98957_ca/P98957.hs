
ones :: [Integer] --crea llista de 1 infinita
ones = iterate (+0) 1

nats :: [Integer] --crea una llista infinita dels naturals
nats = iterate (+1) 0

ints :: [Integer] --Generar la seqüència dels enters [0,1,−1,2,−2,3,−3,4…].
ints = tail $ concat $ map (\x -> [x, -x]) nats

triangulars :: [Integer] --Generar la seqüència dels nombres triangulars: 0,1,3,6,10,15,21,28,…].
triangulars = scanl (+) 0 (tail nats)

factorials :: [Integer] --Generar la seqüència dels nombres factorials: [1,1,2,6,24,120,720,5040,…].
factorials = scanl (*) 1 (tail nats)

fibs :: [Integer] --Generar la seqüència dels nombres de Fibonacci: [0,1,1,2,3,5,8,13,…].
fibs = map fst $ iterate (\(a,b) -> (b, a+b)) (0,1)

primes :: [Integer] --Generar la seqüència dels nombres primers: [2,3,5,7,11,13,17,19,…].
primes = 2:[x | x <- nats,y <-[2..(x-1)] (any ((mod x y) == 0) [2..(x-1)]) == False]
