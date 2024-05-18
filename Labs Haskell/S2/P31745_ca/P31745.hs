 flatten :: [[Int]] -> [Int] --que aplana una llista de llistes d’enters en una llista d’enters.
 flatten [] = []
 flatten l = foldl (++) [] l
 
 myLength :: String -> Int --que retorna la llargada d’una cadena de caràcters.
 myLength s = foldl (\x _ -> x+1) 0 s
 
 myReverse :: [Int] -> [Int] --que inverteix els elements d’una llista d’enters.
 myReverse l = foldl (\xs x -> x:xs) [] l
 
 countIn :: [[Int]] -> Int -> [Int] -- que, donada una llista de llistes d’elements ℓ i un element x ens torna la llista que indica quants cops apareix x en cada llista de ℓ.
 countIn l x = map (length) $ map (filter (== x)) l
 --countIn ll x = foldl (+) 0 $ map (\y -> if y == x then 1 else 0) ll
 
 prueba :: [Int] -> Int -> Int
 prueba ll x = foldl (+) 0 $ map (\y -> if y == x then 1 else 0) ll


 firstWord :: String -> String --que, donat un string amb blancs i caràcacters alfabètics), en retorna la primera paraula.
 firstWord s = takeWhile (/= ' ') $ dropWhile (== ' ') s
 
 
 
