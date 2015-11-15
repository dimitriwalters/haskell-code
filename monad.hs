multiple :: (Num a, Eq a) => a -> a -> Maybe a
multiple 0 _ = Nothing
multiple _ 0 = Nothing
multiple x y = Just (x * y)

foo :: (Num a, Eq a) => Maybe a
foo = do  
    x <- multiple 3 4
    y <- multiple x 6
    z <- multiple y 0
    Just (x + y)
