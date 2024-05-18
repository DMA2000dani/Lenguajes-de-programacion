 absValue :: Int -> Int
 absValue x = if x >= 0 then x else -x


 power :: Int -> Int -> Int
 power _ 0 = 1
 power x y = 
     let z =  power x y_entredos
         y_entredos = div y 2
     in
        if even y then z * z else z * z * x
        
        
 slowFib :: Int -> Int
 slowFib 0 = 0
 slowFib 1 = 1
 slowFib x = slowFib (x-1) + slowFib (x-2)
 
 isPrime :: Int -> Bool
 isPrime 0 = False
 isPrime 1 = False
 isPrime n = not (findDivisor 2)
    where
        findDivisor i 
            | i >= n = False
            | otherwise = mod n i == 0 || findDivisor (i + 1)
