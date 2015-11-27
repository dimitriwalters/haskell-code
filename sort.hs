import Data.List

quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) =
    let (smallerSorted, largerSorted) = partition (<x) xs
    in (quicksort smallerSorted) ++ [x] ++ (quicksort largerSorted)

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergesort :: (Ord a) => [a] -> [a]
mergesort []     = []
mergesort (x:[]) = [x]
mergesort xs =
    let (leftHalf, rightHalf) = splitAt (floor (fromIntegral (length xs) / 2)) xs
        leftSorted            = mergesort leftHalf
        rightSorted           = mergesort rightHalf
    in merge leftSorted rightSorted
