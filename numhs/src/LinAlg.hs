{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-|
Module      : LinAlg
Description : This module implements linear algebra functions to be applied to tensors.
-}
module LinAlg(
    addTensor
) where

import Core
import Math

addTensor :: (Num a) => Tensor a -> Tensor a -> Either Error (Tensor a)
addTensor (Dense v1 s1) (Dense v2 s2) | s1 == s2 = Right $ Dense (zipWith (+) v1 v2) s1
                                      | otherwise = Left $ Error $ "Dimensions " ++ show s1 ++ " are not equal to dimensions " ++ show s2 ++ "."
addTensor _ _ = undefined

dot :: (Num a) => Tensor a -> Tensor a -> Tensor a
dot t1@(Dense v1 [s1]) t2@(Dense v2 [s2]) | s1 == s2 = Dense (zipWith (*) v1 v2) [s1] --0-D 0-D
                                          | otherwise = inner t1 t2 --1-D 1-D
dot t1@(Dense v1 (s11:s12)) t2@(Dense v2 (s21:s22)) = matmul t1 t2 --2-D 2-D
dot t1@(Dense v1 s1) t2@(Dense v2 [s2]) = undefined -- Sum over last axis of a and b
dot t1@(Dense v1 s1) t2@(Dense v2 s2) = undefined -- Sum over last axis of a and second-to-last axis of b
dot _ _ = undefined


inner :: (Num a) => Tensor a -> Tensor a -> Tensor a
inner = undefined

matmul :: (Num a) => Tensor a -> Tensor a -> Tensor a
matmul = undefined

