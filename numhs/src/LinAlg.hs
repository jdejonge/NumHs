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

addTensor :: (Num a) => Tensor a -> Tensor a -> Either Error (Tensor a)
addTensor (Dense v1 s1) (Dense v2 s2) | s1 == s2 = Right $ Dense (zipWith (+) v1 v2) s1
                                      | otherwise = Left $ Error $ "Dimensions " ++ show s1 ++ " are not equal to dimensions " ++ show s2 ++ "."
addTensor _ _ = undefined