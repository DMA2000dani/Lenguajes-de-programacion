data Queue a = Queue [a] [a]
    deriving (Show)

instance (Eq a) => Eq (Queue a) 
    where
        (Queue la1 la2) == (Queue lb1 lb2) = (la1 ++ (reverse la2) ) == (lb1 ++ (reverse lb2))
        
create :: Queue a
create = Queue [] []

push ::  a -> Queue a -> Queue a
push x (Queue l1 l2) = Queue l1 (x:l2)

pop :: Eq a => Queue a -> Queue a
-- pop q@(Queue [] []) = q
-- pop q@(Queue [] l2) = (Queue (tail (reverse l2)) [])
-- pop q@(Queue l1 l2) = (Queue (tail l1) l2)
pop q@(Queue l1 l2)
    | (l1 == []) && (l2 == []) = q
    | l1 == [] = (Queue (tail (reverse l2)) [])
    | otherwise = (Queue (tail l1) l2)

top :: Queue a -> a
-- top q@(Queue [] []) = 
top (Queue [] l2) = last l2
top (Queue l1 _) = head l1

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty q  = False

--  Cua2
instance Functor Queue where
  fmap f (Queue l1 l2) = Queue (map f l1) (map f l2)
  
translation :: Num b => b -> Queue b -> Queue b
translation x q = fmap (+x) q

instance Applicative Queue
    where
        (Queue [x] []) <*> q            = (Queue [] [x]) <*> q
        (Queue [] [x]) <*> q            = (Queue [x] []) <*> q
        pure x                          = (Queue [x] [])

merge :: Queue a -> Queue a -> Queue a
merge (Queue x1 y1) (Queue x2 y2)       = Queue (x1 ++ reverse y1) (x2 ++ reverse y2)

instance Monad Queue 
    where
        return x                        = Queue [x] []
        (Queue x y) >>= f               = foldl merge create (map f (x ++ (reverse y)))  

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter f q = do
    v <- q
    if f v then return v else create
    
