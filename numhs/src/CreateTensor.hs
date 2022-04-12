{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : CreateTensor
Description : This module implements multiple ways to create tensors.
-}
module CreateTensor(
    idMatrix,
    fromList,
    vector,
    constTensor,
    zeros,
    ones
) where


import Data.Kind (Type)
import Data.TypeNums hiding((*), natVal)
import GHC.TypeLits
import Data.Proxy
import Data.Type.Bool

import qualified Data.List as L
import qualified Data.List.Split as LS

import Core

data SizedList a (n :: Nat) where
    SList :: [a] -> SizedList a n deriving(Show)

fromList :: (KnownNat n, ShapeProduct m ~ n, NatVals m) => Length n -> [a] -> Shape m -> Tensor a m
fromList ln lst shp = fromSizedList (fromVanillaList ln lst) shp

vector :: (KnownNat n) => Length n -> [a] -> Tensor a '[n]
vector ln lst = fromList ln lst (Shape :: Shape '[n])


idMatrix :: (Num a) => Length n -> Tensor a '[n, n]
idMatrix = undefined

constTensor :: (NatVals n) => a -> Shape n -> Tensor a n
constTensor a shp = let s = natVals shp in
    Dense (take (product s) (repeat a)) s

zeros :: (NatVals n, Num a) => Shape n -> Tensor a n
zeros = constTensor 0

ones :: (NatVals n, Num a) => Shape n -> Tensor a n
ones = constTensor 1



-- Helper funcitons.

fromSizedList :: (ShapeProduct ns ~ n, NatVals ns) => SizedList a n -> Shape ns -> Tensor a ns
fromSizedList (SList lst) shp = Dense lst (natVals shp)


fromVanillaList :: (KnownNat n) => Length n -> [a] -> SizedList a n
fromVanillaList ln lst  | fromIntegral (natVal ln) == length lst = SList lst
                        | otherwise = error $ "List is annotated with length " ++ show (fromIntegral (natVal ln)) ++ ", but it has length " ++ show (length lst)