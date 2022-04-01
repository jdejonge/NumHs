{-# LANGUAGE RankNTypes #-}

{-|
Module      : Reduce
Description : Dimesion reduction operations.
-}
module Reduce where

import Core (Tensor(..), (|#|), (|@|), Interval(..))

foldDim :: forall a b. (Show a, Show b) => (b -> a -> b) -> Tensor a -> b -> Int -> Tensor b
-- One-dimensional tensor.
foldDim f t@(Dense xs [ln]) acc 0 = let   xs' = foldl f acc xs in
                                        Dense [xs'] [1]
foldDim f t@(Dense xs shape) acc dim = let  slices = foldHelpr [[]] shape dim
                                            threads = map (\s -> case t |#| s of Left e -> error "fix me" ; Right a -> a) slices
                                            simpleFold (Dense d _) = foldl f acc d
                                            simpleFold _ = undefined
                                            folded = map simpleFold threads
                                        in error $ show slices
                                        -- in Dense folded (shapeHelpr dim shape)

shapeHelpr :: Int -> [Int] -> [Int]
shapeHelpr _ [] = []
shapeHelpr 0 (x:xs) = shapeHelpr (-1) xs
shapeHelpr foldIdx (x:xs) = x : (shapeHelpr (foldIdx-1) xs)


foldHelpr :: [[Interval]] -> [Int] -> Int -> [[Interval]]
foldHelpr env [] _ = env
foldHelpr env (x:xs) 0 = let env' = map (\e -> e ++ [Interval 0 x]) env
                            in foldHelpr env' xs (-1)
foldHelpr env (x:xs) n = let    range = [0..x-1]
                                env' = concatMap (\e -> [e ++ [Interval r r] | r <- range]) env
                            in foldHelpr env' xs (n-1)
