{-# LANGUAGE RankNTypes #-}

{-|
Module      : Reduce
Description : Dimesion reduction operations.
-}
module Reduce(
    Reduce.sum,
    sum',
    Reduce.product,
    product',
    mean,
    mean',
    Reduce.minimum,
    minimum',
    Reduce.maximum,
    maximum'
) where

import Data.Default
import Core (Tensor(..), (|#|), (|:|), values, shape, Interval)

foldDim :: forall a b c. (b -> a -> b) -> Tensor a -> b -> Int -> (b -> c) -> Tensor c
-- One-dimensional tensor.
foldDim f t@(Dense xs [ln]) acc 0 wrap = let   xs' = foldl f acc xs in
                                                fmap wrap $ Dense [xs'] [1]
foldDim f t@(Dense xs shape) acc dim wrap = let slices = foldHelpr [[]] shape dim
                                                threads = map (\s -> case t |#| s of Left e -> error "fix me" ; Right a -> a) slices
                                                simpleFold (Dense d _) = foldl f acc d
                                                simpleFold _ = undefined        -- TODO: Define this on sparse tensors!
                                                folded = map simpleFold threads
                                        in fmap wrap $ Dense folded (shapeHelpr dim shape)

shapeHelpr :: Int -> [Int] -> [Int]
shapeHelpr _ [] = []
shapeHelpr 0 (x:xs) = shapeHelpr (-1) xs
shapeHelpr foldIdx (x:xs) = x : (shapeHelpr (foldIdx-1) xs)


foldHelpr :: [[Interval]] -> [Int] -> Int -> [[Interval]]
foldHelpr env [] _ = env
foldHelpr env (x:xs) 0 = let env' = map (\e -> e ++ [0 |:| x]) env
                            in foldHelpr env' xs (-1)
foldHelpr env (x:xs) n = let    range = [0..x-1]
                                env' = concatMap (\e -> [e ++ [r |:| (r+1)] | r <- range]) env
                            in foldHelpr env' xs (n-1)

-- |Calculates the sum of the tensor along the specified dimension. The resulting tensor will be collapsed
-- along the specified dimension, with the dimension disappearing from the shape. 
-- 
-- __Examples__
-- 
-- For example if @shape t == [2, 3]@
-- then @shape (sum t 0) == [3]@ and @shape (sum t 1) == [2]@.
sum :: (Num a) => Tensor a      -- ^The tensor to take the sum of.
        -> Int                  -- ^The dimension along which to take the sum.
        -> Tensor a             -- ^Returns the tensor with the sums, where the specified dimension is collapsed.
sum t i = foldDim (+) t 0 i id


-- |Calculates the sum of all elements of the tensor. It is equivalent to applying @sum tensor 0@ to a tensor
-- repeatedly, until there is only a single element left.
sum' :: (Num a) => Tensor a     -- ^The tensor to take the sum of.
        -> Tensor a             -- ^The resulting tensor will have a shape of @[1]@, and a value that is the
                                -- sum of the elements of the original tensor.
sum' t  | length (shape t) == 1 = Reduce.sum t 0
        | otherwise = sum' (Reduce.sum t 0)

-- |Calculates the product of the tensor along the specified dimension. The resulting tensor will be collapsed
-- along the specified dimension, with the dimension disappearing from the shape. 
-- 
-- __Examples__
-- 
-- For example if @shape t == [2, 3]@
-- then @shape (product t 0) == [3]@ and @shape (product t 1) == [2]@.
product :: (Num a) => Tensor a  -- ^The tensor to take the product of.
            -> Int              -- ^The dimension along which to take the product.
            -> Tensor a         -- ^Returns the tensor with the products, where the specified dimension is collapsed.
product t i = foldDim (*) t 1 i id


product' :: (Num a) => Tensor a
            -> Tensor a
product' t  | length (shape t) == 1 = Reduce.product t 0
            | otherwise = product' (Reduce.product t 0)


mean :: (Fractional a) => Tensor a
        -> Int
        -> Tensor a
mean t i = let  f (counter, avg) element = (counter + 1, avg + (1 / counter) * (element - avg))
                acc = (1, 0)
            in foldDim f t acc i snd

mean' :: (Fractional a) => Tensor a
        -> Tensor a
mean' t | length (shape t) == 1 = Reduce.mean t 0
        | otherwise = mean' (Reduce.mean t 0)

maximum :: (Ord a, Default a) => Tensor a
            -> Int
            -> Tensor a
maximum t i = let   first = head $ values t
                in foldDim (\m e -> max m e) t first i id


maximum' :: (Ord a, Default a) => Tensor a
        -> Tensor a
maximum' t  | length (shape t) == 1 = Reduce.maximum t 0
            | otherwise = maximum' (Reduce.maximum t 0)


minimum :: (Ord a, Default a) => Tensor a
            -> Int
            -> Tensor a
minimum t i = let   first = head $ values t
                in foldDim (\m e -> min m e) t first i id

minimum' :: (Ord a, Default a) => Tensor a
        -> Tensor a
minimum' t  | length (shape t) == 1 = Reduce.minimum t 0
            | otherwise = minimum' (Reduce.minimum t 0)

