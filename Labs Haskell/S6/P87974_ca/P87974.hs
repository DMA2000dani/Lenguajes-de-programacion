main :: IO ()
-- main = do
--     contents <- readFile "exemple.txt"
--     putStrLn contents
-- lines :: String -> [String]

--Del Revés
-- main = do
--     x <- getLine
--     let y = reverse x
--     putStrLn x
--     putStrLn y

-- Hello World
-- main = do
--     putStrLn "Com et dius?"
--     nom <- getLine
--     putStrLn $ "Hola " ++ nom ++ "!"

main = do
    nom <- getLine
    print nom
    putStrLn nom
