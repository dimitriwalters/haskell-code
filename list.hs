import Data.List

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let (smallerSorted, largerSorted) = partition (<x) xs
    in (quicksort smallerSorted) ++ [x] ++ (quicksort largerSorted)
