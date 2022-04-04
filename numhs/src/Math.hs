{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-|
Module      : Math
Description : This module implements math functions to be applied to tensors.
-}
module Math(
    tensorMultiply
) where

import Core

tensorMultiply :: (Num a) -> Tensor a -> Tensor a -> Either Error (Tensor a)
tensorMulitply t1@(Dense v1 s1) t2@(Dense v2 s2) | s1 == s2 = zipWithTensor (*) t1 t2
                                     | otherwise = Left $ Error $ "Dimensions " ++ show s1 ++ " are not equal to dimensions " ++ show s2 ++ "."