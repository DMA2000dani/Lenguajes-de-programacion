fizzBuzz :: [Either Int String] 
fizzBuzz map fizzBuzz' [0..]

fizzBuzz' :: Int -> Either Int String
fizzBuzz' num 
    | (mod num 3) && (mod num 5) = Right "FizzBuzz"
    | mod num 3 = Right "Fizz"
    | mod num 5 = Right "Buzz"
    | otherwise = Left num
