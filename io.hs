printList :: (Show a) => [a] -> IO()
printList = mapM_ print
