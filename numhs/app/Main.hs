module Main where

import Core
import CreateTensor
import LinAlg
import Math

main :: IO ()
main = do
    -- Indexing
    --let (Right vec) = fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27] [3, 3, 3] -- Creating a 3D array of shape [2, 2, 2]
    --putStrLn $ "3D tensor: " ++ show vec ++ ". Indexed at [0, 1, 1] = " ++ show (vec |@| [0, 1, 1])

    --let idtensor = idTensor 3 3
    --putStrLn $ "ID tensor: " ++ show idtensor ++ "."


    --let (Right vec1) = fromList [1, 2, 3, 4, 5, 6] [2, 3]
    --let (Right vec2) = fromList [7, 8, 9, 10, 11, 12] [3, 2]

    let (Right vec3) = fromList [1, 7, 3, 9, 5, 11, 2, 8, 4, 10, 6, 12] [2, 3, 2]
    print vec3
    print $ getIndex vec3 [1, 1, 1]
    print (test vec3 0 [1, 1])
    print (getDim vec3 0 [1, 1])
    --let (Right result) = matmul vec1 vec2
    --putStrLn $ show $ values result

    -- -- Slicing
    -- let (Right vec') = fromList [1..9] [3, 3]
    -- -- Taking entries from first two row and last two column.
    -- let (Right vec'') = vec' |#| [0|:|2, 1|:|3]
    -- putStrLn $ "3x3 matrix " ++ show vec' ++ " sliced with [0|:|2, 1|:|3] results in 2x2 matrix " ++ show vec'' ++ " with shape " ++ show (shape vec'')

