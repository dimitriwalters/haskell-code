multiple :: (Num a, Eq a) => a -> a -> Maybe a
multiple 0 _ = Nothing
multiple _ 0 = Nothing
multiple x y = Just (x * y)
