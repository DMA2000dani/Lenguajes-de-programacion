data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add ex ey) = (eval1 ex) + (eval1 ey)
eval1 (Sub ex ey) = (eval1 ex) - (eval1 ey)
eval1 (Mul ex ey) = (eval1 ex) * (eval1 ey)
eval1 (Div ex ey) = div (eval1 ex) (eval1 ey)


eval2 :: Expr -> Maybe Int
eval2 (Val x) = return x
eval2 (Add ex ey) = do
    x <- eval2 ex
    y <- eval2 ey
    Just (x + y)
eval2(Sub ex ey) = do
    x <- eval2 ex
    y <- eval2 ey
    return $ x - y
eval2(Mul ex ey) = do
    x <- eval2 ex
    y <- eval2 ey
    return $ x * y
eval2(Div ex ey) = do
    x <- eval2 ex
    y <- eval2 ey
    if y == 0 then Nothing else Just (div x y)

eval3 :: Expr -> Either String Int
eval3 (Val x) = Right x
eval3 (Add ex ey) = do
    x <- eval3 ex
    y <- eval3 ey
    return & x + y
eval3 (Sub ex ey) = do
    x <- eval3 ex
    y <- eval3 ey
    Right (x-y)
eval3 (Mul ex ey) = do
    x <- eval3 ex
    y <- eval3 ey
    Right (x-y)
eval3 (Div ex ey) = do
    x <- eval3 ex
    y <- eval3 ey
    if y == 0 then Left "div0" else return $ (div x y)
    
    
