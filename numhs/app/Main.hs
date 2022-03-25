module Main where

import Core


main :: IO ()
main = print "Hello"
    -- Indexing
    -- let (Right vec) = fromList [1, 2, 3, 4, 5, 6, 7, 8] [2, 2, 2] -- Creating a 3D array of shape [2, 2, 2]
    -- putStrLn $ "3D tensor: " ++ show vec ++ ". Indexed at [0, 1, 1] = " ++ show (vec |@| [0, 1, 1])

    -- -- Slicing
    -- let (Right vec') = fromList [1..9] [3, 3]
    -- -- Taking entries from first two row and last two column.
    -- let (Right vec'') = vec' |#| [0|:|2, 1|:|3]
    -- putStrLn $ "3x3 matrix " ++ show vec' ++ " sliced with [0|:|2, 1|:|3] results in 2x2 matrix " ++ show vec'' ++ " with shape " ++ show (shape vec'')

    -- let (Right vec''') = vec' |#| [0|:|1, 0|:|3]
    -- putStrLn $ "Another example with the same matrix: " ++ show vec' ++ " |#| [0|:|1, 0|:|3] results in the first row vector " ++ show vec'''
    
