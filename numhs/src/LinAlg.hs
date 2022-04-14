{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-|
Module      : LinAlg
Description : This module implements linear algebra functions to be applied to tensors.
-}
module LinAlg(
    addTensor,
    matmul,
    getRow2D,
    getColumn2D,
    getDim
) where

import Core
import Math
import Data.Maybe
import Data.Either

addTensor :: (Num a) => Tensor a -> Tensor a -> Either Error (Tensor a)
addTensor (Dense v1 s1) (Dense v2 s2) | s1 == s2 = Right $ Dense (zipWith (+) v1 v2) s1
                                      | otherwise = Left $ Error $ "Dimensions " ++ show s1 ++ " are not equal to dimensions " ++ show s2 ++ "."
addTensor _ _ = undefined

dot :: (Num a) => Tensor a -> Tensor a -> Either Error (Tensor a)
dot t1@(Dense v1 [s1]) t2@(Dense v2 [s2]) = Right $ Dense (zipWith (*) v1 v2) [smallest]
    where smallest = min s1 s2
--dot t1@(Dense v1 (s11:s12)) t2@(Dense v2 (s21:s22)) = Right $ matmul t1 t2 --2-D 2-D
dot t1@(Dense v1 _) t2@(Dense v2 _) = Left $ Error "Higher dimensional than 2 cannot be possible."
dot _ _ = undefined

replaceElement :: [a] -> a -> Int -> [a]
replaceElement xs new i = x ++ new : ys
    where (x,_:ys) = splitAt i xs

addDummyElement :: [a] -> a -> Int -> [a]
addDummyElement xs dummy i = x ++ dummy : y
    where (x,y) = splitAt i xs

-- |Gets a dimension vector given a dimension and coordinates on that dimension.
getDim :: Tensor a -- ^`Tensor` for which the dimension should be found.
        -> Int -- ^Dimension to be gotten.
        -> [Int] -- ^Location of dimension to be gotten, should be a value for every element in shape except for the dimension.
        -> Either Error [a] --Dimension
getDim t@(Dense v s) x c | length maybeList == length noMaybeList = Right noMaybeList
                         | otherwise = Left $ Error "Incorrect indices."
    where noMaybeList = catMaybes maybeList
          maybeList = map (index shapeList x t) (increasingList (s !! x))
          shapeList = addDummyElement c 0 x
          index ls i t x | isRight y = let (Right ys) = y in Just ys
                         | otherwise = Nothing
                        where y = t |@| replaceElement ls x i
                        

getRow2D :: (Num a) => Tensor a  -> Int -> Either Error [a]
getRow2D t@(Dense xs [d1, d2]) r = getDim t 0 [r]
getRow2D _ _ = Left $ Error "Only defined for 2 dimensional matrices. Use getDim instead."

getColumn2D :: (Num a) => Tensor a -> Int -> Either Error [a]
getColumn2D t@(Dense x [d1, d2]) c = getDim t 1 [c]
getColumn2D _ _ = Left $ Error "Only defined for 2 dimensional matrices. Use getDim instead."

dotVector :: (Num a) => [a] -> [a] -> a
dotVector x y = sum $ zipWith (*) x y

tensorDotIndex :: (Num a) => Tensor a -> Tensor a -> Int -> Int -> a
tensorDotIndex t1 t2 row col = dotVector rowVec colVec
  where (Right rowVec) = getRow2D t1 row
        (Right colVec) = getColumn2D t2 col

inner :: (Num a) => Tensor a -> Tensor a -> Tensor a
inner = undefined

matmul :: (Num a) => Tensor a -> Tensor a -> Either Error (Tensor a)
matmul t1@(Dense v1 s1@[s11, s12]) t2@(Dense v2 s2@[s21, s22]) | s11 == s22 = Right $ Dense (loopOverCol (s11 - 1) (s11 - 1)) [s11, s11]
                                                         | otherwise = Left $ error $ "shapes " ++ show s1 ++ " and " ++ show s2 ++ " are not compatible."
    where loopOverRow row col | row < 0 = []
                              | otherwise = tensorDotIndex t1 t2 row col : loopOverRow (row - 1) col
          loopOverCol maxRow col | col < 0 = []
                                 | otherwise = loopOverRow maxRow col ++ loopOverCol maxRow (col - 1)
matmul _ _ = error "Only defined for 2 dimensionl matrices"

concat :: Tensor a -> Tensor a -> Int -> Tensor a
concat (Dense x xs) (Dense xy ys) a = undefined 
concat _ _ _ = undefined
