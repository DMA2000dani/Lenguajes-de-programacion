data Tree a = Node a (Tree a) (Tree a)  | Empty 
    deriving (Show)
     
size :: Tree a -> Int --que, donat un arbre, retorni la seva talla, és a dir, el nombre de nodes que conté
size Empty = 0
size (Node _ fe fd) = 1 + size fe + size fd

height :: Tree a -> Int --que, donat un arbre, retorni la seva alçada, assumint que els arbres buits tenen alçada zero.
height Empty = 0
height (Node _ ae ad) = 1 + max(size ae)  (size ad)

equal :: Eq a => Tree a -> Tree a -> Bool --que, donat dos arbres, indiqui si són el mateix.
equal Empty Empty = True
equal (Node x1 fe1 fd1) (Node x2 fe2 fd2) = if(x1 /= x2) then False else equal fe1 fe2 && equal fd1 fd2

isomorphic :: Eq a => Tree a -> Tree a -> Bool --que, donat uns arbres, indiqui si són el isomorfs, és a dir, si es pot obtenir l’un de l’altre tot girant algun dels seus fills.
isomorphic Empty Empty = True
isomorphic (Node x fe1 fd1) (Node x2 fe2 fd2) = if(x == x2) && ((equal fe1 fd2 && equal fd1 fe2) ||(equal fd1 fd2 && equal fe1 fe2)) then True else False

preOrder :: Tree a -> [a] --que, donat un arbre, retorni el seu recorregut en pre-ordre.
preOrder Empty = []
preOrder (Node x fe fd) = [x] ++ preOrder fe ++ preOrder fd -- amb els dos punt no deixa ja que no es pot fer 1:[]:[] en canvi si 1++[]++[]

postOrder :: Tree a -> [a] --que, donat un arbre, retorni el seu recorregut en post-ordre.
postOrder Empty = []
postOrder (Node x fe fd) = preOrder fe ++ preOrder fd ++ [x]

inOrder :: Tree a -> [a] --que, donat un arbre, retorni el seu recorregut en in-ordre.
inOrder Empty = []
inOrder (Node x fe fd) = preOrder fe ++ [x] ++ preOrder fd

bfs [] = []
bfs (Empty:xs) = bfs xs
bfs ((Node x l r):xs) = x : (bfs $ xs ++ [l,r])

breadthFirst :: Tree a -> [a]
breadthFirst t = bfs [t]
