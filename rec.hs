dot :: (Num a) => [a] -> [a] -> a
dot [] _ = 0
dot _ [] = 0
dot (x:xs) (y:ys) = x * y + dot xs ys

sum' :: (Num a) => [a] -> a
sum' = foldl1 (+)

fact :: Int -> Int
fact n
    | n <= 0    = 1
    | otherwise = foldl1 (*) [1..n]
