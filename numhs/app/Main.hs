module Main where

import Core


main :: IO ()
main = do
    let (Right vec) = fromList [1, 2, 3, 4, 5, 6, 7, 8] [2, 2, 2] -- Creating a 3D array of shape [2, 2, 2]
    print $ vec
    print $ vec # [0, 1, 1] -- Indexing
