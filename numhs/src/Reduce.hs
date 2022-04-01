{-# LANGUAGE RankNTypes #-}

{-|
Module      : Reduce
Description : Dimesion reduction operations.
-}
module Reduce where

import Core (Tensor(..), (|#|), (|:|), Interval(..), values, shape)

foldDim :: forall a b. (b -> a -> b) -> Tensor a -> b -> Int -> Tensor b
-- One-dimensional tensor.
foldDim f t@(Dense xs [ln]) acc 0 = let   xs' = foldl f acc xs in
                                        Dense [xs'] [1]
foldDim f t@(Dense xs shape) acc dim = let  slices = foldHelpr [[]] shape dim
                                            threads = map (\s -> case t |#| s of Left e -> error "fix me" ; Right a -> a) slices
                                            simpleFold (Dense d _) = foldl f acc d
                                            simpleFold _ = undefined        -- TODO: Define this on sparse tensors!
                                            folded = map simpleFold threads
                                        in Dense folded (shapeHelpr dim shape)

shapeHelpr :: Int -> [Int] -> [Int]
shapeHelpr _ [] = []
shapeHelpr 0 (x:xs) = shapeHelpr (-1) xs
shapeHelpr foldIdx (x:xs) = x : (shapeHelpr (foldIdx-1) xs)


foldHelpr :: [[Interval]] -> [Int] -> Int -> [[Interval]]
foldHelpr env [] _ = env
foldHelpr env (x:xs) 0 = let env' = map (\e -> e ++ [0 |:| x]) env
                            in foldHelpr env' xs (-1)
foldHelpr env (x:xs) n = let    range = [0..x-1]
                                env' = concatMap (\e -> [e ++ [r |:| r+1] | r <- range]) env
                            in foldHelpr env' xs (n-1)


sum :: (Num a) => Tensor a
        -> Int
        -> Tensor a
sum t i = foldDim (+) t 0 i


sum' :: (Num a) => Tensor a
        -> Tensor a
sum' t  | length (shape t) == 1 = Reduce.sum t 0
        | otherwise = sum' (Reduce.sum t 0)

product :: (Num a) => Tensor a
            -> Int
            -> Tensor a
product t i = foldDim (*) t 1 i


product' :: (Num a) => Tensor a
            -> Tensor a
product' t  | length (shape t) == 1 = Reduce.product t 1
            | otherwise = product' (Reduce.product t 1)


mean :: (Num a, Fractional a) => Tensor a
        -> Int
        -> Tensor a
mean t i = let  f (counter, avg) element = (counter + 1, avg + (1 / counter) * (element - avg))
                acc = (1, 0)
                out = foldDim f t acc i
                v = values out
                s = shape out
                v' = map snd v
            in Dense v' s

maximum :: (Ord a) => Tensor a
            -> Int
            -> Tensor a
maximum t i = let   first = head $ values t
                in foldDim (\m e -> max m e) t first i


minimum :: (Ord a) => Tensor a
            -> Int
            -> Tensor a
minimum t i = let   first = head $ values t
                in foldDim (\m e -> min m e) t first i


