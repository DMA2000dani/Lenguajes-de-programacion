myMap :: (a -> b) -> [a] -> [b] --que emuli el map usant llistes per comprensió.
myMap op l = [op x | x <- l]


myFilter :: (a -> Bool) -> [a] -> [a] -- que emuli el filter usant llistes per comprensió.
myFilter b l = [x | x <- l, b x]
-- myFilter b [] = []
-- myFilter b (l:ls)  
--     | b l = l:myFilter b ls 
--     | otherwise = myFilter b ls
    
    
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] -- que que emuli el zipWith usant llistes per comprensió i zip.
myZipWith op la lb = [op x y | (x,y)<-zip la lb]

thingify :: [Int] -> [Int] -> [(Int, Int)] --que, donades dues llistes d’enters, genera la llista que aparella els elements si l’element de la segona llista divideix al de la primera.
thingify la lb = [(x,y) | x<-la, y <- lb, mod x  y == 0]

factors :: Int -> [Int] --que, donat un natural no nul, genera la llista ordenada amb els seus factors (no necessàriament primers).
factors num = [x | x <- [1..num], mod num x == 0] 
