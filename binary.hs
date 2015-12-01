data Binary = Zero | One deriving (Show, Eq)

binaryFlip :: [Binary] -> [Binary]
binaryFlip [] = []
binaryFlip (x:xs)
    | x == Zero = One : (binaryFlip xs)
    | otherwise = Zero : (binaryFlip xs)
