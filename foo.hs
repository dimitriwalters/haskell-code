import Control.Monad

print4 :: IO ()
print4 = do { print 4 }

print1To10 :: IO ()
print1To10 = forM_ [1..10] print

sum1To10 :: Int
sum1To10 = foldl (+) 0 [1..10]

first :: [a] -> a
first (x:xs) = x
