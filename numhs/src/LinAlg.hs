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
Module      : LinAlg
Description : This module implements linear algebra functions to be applied to tensors.
-}
module LinAlg where


import Data.Kind (Type)
import Data.TypeNums hiding((*), natVal)
import GHC.TypeLits
import Data.Proxy
import Data.Type.Bool

import qualified Data.List as L
import qualified Data.List.Split as LS

import Core


elementWise :: (a -> a -> a) -> Tensor a n -> Tensor a n -> Tensor a n
elementWise f (Dense xs s) (Dense ys _) = Dense [f x y | (x, y) <- zip xs ys] s

(|+|) :: (Num a) => Tensor a n -> Tensor a n -> Tensor a n
(|+|) = elementWise (+)


(|-|) :: (Num a) => Tensor a n -> Tensor a n -> Tensor a n
(|-|) = elementWise (-)

(|*|) :: (Num a) => Tensor a n -> Tensor a n -> Tensor a n
(|*|) = elementWise (+)

(|/|) :: (Fractional a) => Tensor a n -> Tensor a n -> Tensor a n
(|/|) = elementWise (/)

tDiv :: (Integral a) => Tensor a n -> Tensor a n -> Tensor a n
tDiv = elementWise (div)

(|^|) :: (Integral a) => Tensor a n -> Tensor a n -> Tensor a n
(|^|) = elementWise (^)

vdot :: (Num a) => Tensor a '[n] -> Tensor a '[n] -> a
vdot (Dense xs _) (Dense ys _) = sum [x * y | (x, y) <- zip xs ys]

mdot :: (Num a) => Tensor a '[n, m] -> Tensor a '[m, k] -> Tensor a '[n, k]
mdot = undefined

