    insert :: [Int] -> Int -> [Int]
    insert [] x = [x]
    insert l x = if head l > x then x : l  else head l : insert (tail l) x
   
    isort :: [Int] -> [Int]
    isort [] = []
    isort [x] = [x]
    isort (xl:l) = if xl > head l then insert l xl else xl : isort l
    
    elimina :: [Int] -> Int -> [Int] -- Treu de la llista l'element x - esim
    elimina [] x = []
    elimina l 0 = drop 1 l
    elimina l x = if(length l == x) then take (x-1) l else (take (x-1) l) ++ (drop x l)
    
    buscapos :: [Int] -> Int -> Int -- pre és si o si. busca la posició de l'element x a la llista
    buscapos [] x = -1
    buscapos (la:las) x = if la == x then 1 else 1 + buscapos las x
    
    remove ::  [Int] -> Int -> [Int]
    remove [] _ = []
    remove l x = elimina l (buscapos l x)
    
    ssort :: [Int] -> [Int]
    ssort [] = []
    ssort l = [minimum l] ++ ssort (remove l (minimum l))
    
    merge :: [Int] -> [Int] -> [Int]
    merge [] lb = lb
    merge la [] = la
    merge laa@(la:las) lbb@(lb:lbs) = if la > lb then lb : merge laa lbs else la : merge lbb las
    
    msort :: [Int] -> [Int]
    msort [] = []
    msort [x] = [x]
    msort l = merge (msort la) (msort lb)
        where la = take meitat l
              lb = drop meitat l
              meitat = (div (length l) 2)

