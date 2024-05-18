 module Main where
 myLength :: [Int] -> Int
 myLength [] = 0
 myLength xs =  1 + myLength (tail xs)

 eql :: [Int] -> [Int] -> Bool --que indiqui si dues llistes d’enters són iguals.
 eql la lb = myLength la == myLength lb && all (==True) (zipWith (==) la lb)
 
 prod :: [Int] -> Int --que calculi el producte dels elements d’una llista d’enters.
 prod la = foldl (*) 1 la
 
 prodOfEvens :: [Int] -> Int --que multiplica tots el nombres parells d’una llista d’enters.
 prodOfEvens la = foldl (*) 1 (filter  even  la)
 
 powersOf2 :: [Int] --que generi la llista de totes les potències de 2.
 powersOf2 = iterate (*2) 1 
 
 scalarProduct :: [Float] -> [Float] -> Float --que calculi el producte escalar de dues llistes de reals de la mateixa mida.
 scalarProduct la lb = foldl (+) 0 $ zipWith (*) la lb
