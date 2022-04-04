{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-|
Module      : CreateTensor
Description : This module implements multiple ways to create tensors.
-}
module CreateTensor(
    idTensor
) where

import Core

idTensor :: Int -> Int -> Tensor Int
idTensor size dimension = Dense (idTensorVals (gap size size (dimension' - 1)) size) (replicate dimension' size)
    where dimension' = dimension - 1
          gap val len 0 = val
          gap val len i = gap ((val + 1) * len) len (i - 1)
    
idTensorVals :: Int -> Int -> [Int]
idTensorVals gap 1 = [1]
idTensorVals gap size = [1] ++ addGap gap (idTensorVals gap (size - 1))
    where addGap 0 xs = xs
          addGap i xs = [0] ++ addGap (i - 1) xs