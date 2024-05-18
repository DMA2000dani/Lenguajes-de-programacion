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


