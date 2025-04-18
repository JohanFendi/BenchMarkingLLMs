

main :: IO()
main = do 
    str <- getLine
    str2 <- getLine
    putStrLn $ show $ fib $ read str
    putStrLn $ show $ fib $ read str2
   

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)