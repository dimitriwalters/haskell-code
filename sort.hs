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

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (floor (fromIntegral (length xs) / 2)) xs

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

mergesort :: (Ord a) => [a] -> [a]
mergesort []     = []
mergesort (x:[]) = [x]
mergesort xs =
    let (left, right) = mapTuple mergesort $ splitHalf xs
    in merge left right
