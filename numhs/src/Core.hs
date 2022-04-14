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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Core
Description : This module describes the core data types and functions of NumHS.
-}
module Core(
    Tensor(Dense),
    shape,
    values,
    reshape,
    (|@|),
    (|#|),
    ShapeProduct,
    NatVals(..),
    Length(..),
    Shape(..)
) where

import Data.Kind (Type)
import Data.TypeNums hiding((*), natVal)
import GHC.TypeLits
import Data.Proxy
import Data.Type.Bool

import qualified Data.List as L
import qualified Data.List.Split as LS

data Shape (n :: [Nat]) where
    Shape :: Shape n deriving(Show)

data Length (n :: Nat) where
    Length :: Length n deriving(Show)

data Index (n :: [Nat]) where
    Index :: Index n deriving(Show)


-- A slice is a list of pair of indices, eg [[1, 4], [5, 6], [7, 12]]... etc
data Slice (i :: [[Nat]]) where
    Slice :: Slice i deriving(Show)

data Tensor a (n :: [Nat]) where
    Dense :: [a] -> [Int] -> Tensor a n

shape :: Tensor a n -> [Int]
shape (Dense _ s) = s

values :: Tensor a n -> [a]
values (Dense v _) = v

instance (Show a) => Show (Tensor a n) where
    show (Dense xs shp) = reclstprint xs shp where
        reclstprint xs [] = show xs
        reclstprint xs [_] = show xs
        reclstprint xs (s:shape) = let  chunks = LS.chunksOf (length xs  `div` s) xs
                                        smalls = map ((flip reclstprint) shape) chunks
                                        inside = L.intercalate ", " smalls
                                    in "[" ++ inside ++ "]"

type family ShapeProduct (n :: [Nat]) :: Nat where
    ShapeProduct '[] = 1
    ShapeProduct (x ': xs) = (GHC.TypeLits.*) x (ShapeProduct xs)

-- First is shape, second is index.
type family ValidIndex' (sh :: [Nat]) (i :: [Nat]) :: Bool where
    ValidIndex' '[] '[] = 'True
    ValidIndex' (x ': xs) (y ': ys) = (Data.Type.Bool.&&) (Not ((Data.TypeNums.<=?) x y)) (ValidIndex' xs ys)
    ValidIndex' _ _ = 'False

type ValidIndex n i = ValidIndex' n i ~ 'True

type family ValidSlice' (sh :: [Nat]) (sl :: [[Nat]]) :: Bool where
    ValidSlice' '[] ('[ '[] ]) = 'True
    -- 0 <= start < end <= x
    ValidSlice' (x ': xs) ([start, end] ': ys) = (Data.Type.Bool.&&) 
                                                    ( (Data.Type.Bool.&&) ( (Data.TypeNums.<=?) 0 start ) (Not ((Data.TypeNums.<=?) end start)) )
                                                    ( (Data.Type.Bool.&&) ((Data.TypeNums.<=?) end x) (ValidSlice' xs ys) )
    ValidSlice' _ _ = 'True

type ValidSlice sh sl = ValidSlice' sh sl ~ 'True

type family SliceShape (sl :: [[Nat]]) :: [Nat] where
    SliceShape ('[ '[] ]) = '[0]
    SliceShape ('[start, end] ': xs) = ((GHC.TypeLits.-) end start) ': (SliceShape xs)


-- This type class produces a list of integers from a type-level Nat list.
class NatVals a where
    natVals :: proxy a -> [Int]

instance NatVals '[] where
    natVals _ = []

instance (KnownNat x, NatVals xs) => NatVals (x ': xs) where
    natVals _ = fromIntegral (natVal (Length :: Length x)) : natVals (Shape :: Shape xs)

class IntervalVals a where
    intervalVals :: proxy a -> [[Int]]

instance IntervalVals '[] where
    intervalVals _ = [[]]

instance (NatVals x, IntervalVals xs) => IntervalVals (x ': (xs :: [[Nat]])) where
    intervalVals _ = (natVals (Shape :: Shape x)) : (intervalVals (Slice :: Slice xs))


reshape :: (ShapeProduct n ~ ShapeProduct m, NatVals m) => Tensor a n -> Shape m -> Tensor a m
reshape (Dense d s) s' = Dense d (natVals s')

(|@|) :: (ValidIndex n m, NatVals m) => Tensor a n -> Index m -> a
(Dense xs []) |@| _ = undefined
(Dense xs s@(_:shape)) |@| index' = let index = natVals index'
                                        idx = sum [a * b |(a, b) <- zip (shape ++ [1]) index]
                            in xs !! idx

(|#|) :: (ValidSlice n slc, SliceShape slc ~ m, IntervalVals slc) => Tensor a n -- ^The tensor to take the subtensor of.
        -> Slice slc -- ^List of intervals for each dimension to be included in the new tensor.
        -> Tensor a m -- ^Returns a new `Tensor` of the same dimensionality on succes.
(Dense d shape) |#| intervals' = let
                                    intervals = intervalVals intervals'
                                    mods = shape
                                    divs = foldr (\e a -> ((head a * e) : a)) [1] (tail shape)
                                    flatToNested = map (\(m, d) -> \x -> x `div` d `mod` m) (zip mods divs)
                                    flatIdx = [0..(length d)-1]
                                    nested = map (\i -> map ($i) flatToNested) flatIdx -- [[dim0, dim1, dim2], [dim0, dim1, dim2]]
                                    nestedData = zip nested d
                                    filtered = filter (\(n, x) -> all (\(idx, inter) -> inInterval inter idx) (zip n intervals)) nestedData
                                    new_data = [x | (_, x) <- filtered]

                                    inInterval :: [Int] -> Int -> Bool
                                    inInterval [lower, upper] n = lower <= n && n < upper

                                    new_shape = map (\[lower, upper] -> upper - lower) (filter (\e -> length e > 0) intervals)
                                in Dense new_data new_shape
