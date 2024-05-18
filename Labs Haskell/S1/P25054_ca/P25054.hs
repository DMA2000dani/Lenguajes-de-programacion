 myLength :: [Int] -> Int
 myLength [] = 0
 myLength xs =  1 + myLength (tail xs)
 
 myMax :: Int -> Int -> Int
 myMax x y = if x > y then x else y
 
 myMaximum ::  [Int] -> Int
 myMaximum [x] = x
 myMaximum (y:ys) =
    let maxim = myMax y (myMaximum  ys)
    in  if myLength ys == 1 then myMax y (head ys) else maxim
    

 suma :: [Int] -> Float
 suma [] = 0
 suma (x:xs) = fromIntegral x + suma xs

 average :: [Int] -> Float
 average xs = ((suma xs) / (fromIntegral (myLength xs)))
 
 buildPalindrome :: [Int] -> [Int]
 buildPalindrome xs = xs ++ (reverse xs)
 
--  elimina :: [Int] -> Int -> [Int] -- Treu de la llista l'element x - esim
--  elimina [] x = []
--  elimina l 0 = drop 1 l
--  elimina l x = if(myLength l == x) then take (x-1) l else (take (x-1) l) ++ (drop x l)
--  
--  buscapos :: [Int] -> Int -> Int -- pre és si o si. busca la posició de l'element x a la llista
--  buscapos [] x = -1
--  buscapos (la:las) x = if la == x then 1 else 1 + buscapos las x
--  
--  treu :: [Int] -> Int -> [Int] -- indica si hi és l'element x a la llista
--  treu [] _ = []
--  treu la x = if elem x la then elimina la (buscapos la x) else la
--  
--  remove :: [Int] -> [Int] -> [Int] -- donades dues llistes, elimina els elements de la segona llista als de la primera
--  remove la [] = la
--  remove [] _ = []
--  remove la (lb:lbs) = remove (treu la lb) lbs

 remove :: [Int] -> [Int] -> [Int]
 remove la [] = la
 remove [] _ = []
 remove la (lb:lbs) = remove (treu la lb) lbs
    where
        treu li i 
            | li == [] = []
            | elem i li =  elimina li (buscapos li i) 
            | otherwise li
            where 
                buscapos lj j
                    | lj == [] = -1
                    | otherwise 1 + buscapos lj i
                elimina lk k  
                    | lk == [] = lk
                    | k == 0 = drop 1 lk
                    | otherwise if (myLength lk == k) then take (k-1) l else (take (k-1) lk) ++ (drop k lk)

 
--  flatten :: [[Int]] -> [Int]
--  flatten (x:xs) = x ++ flatten xs

 oddsNevens :: [Int] -> ([Int], [Int])
 oddsNevens [] = ([], [])
 oddsNevens (x : xs)
    | even x = (a, x : b)
    | otherwise = (x : a, b)
    where (a, b) = oddsNevens xs
  
     
