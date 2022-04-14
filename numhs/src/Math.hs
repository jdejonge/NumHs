{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : Math
Description : This module implements math functions to be applied to tensors.
-}
module Math(
    tensorMultiply,
    increasingList
) where

import Core
import Data.List

tensorMultiply :: Num a => Tensor a -> Tensor a -> Either Error (Tensor a)
tensorMultiply t1@(Dense v1 s1) t2@(Dense v2 s2) 
    | s1 == s2 = Right $ Dense (zipWith (*) v1 v2) s1
    | otherwise = Left $ Error $ "Dimensions " ++ show s1 ++ " are not equal to dimensions " ++ show s2 ++ "."
tensorMultiply _ _ = undefined

zipWithTensor :: (a0 -> a0 -> a0) -> Tensor a -> Tensor a -> Either Error (Tensor a)
zipWithTensor = error "not implemented"

increasingList :: Int -> [Int]
increasingList y = unfoldr (f y) 0
    where f y x | x < y = Just (x, x+1)
                | otherwise = Nothing