{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-|
Module      : Core
Description : This module describes the core data types and functions of NumHS.
-}
module Core(
    Error(..),
    Tensor(Dense, Sparse),
    vector,
    fromList,
    shape,
    values,
    reshape,
    (|@|),
    (|:|),
    (|#|),
    listToListN,
    mapTensor
) where

import qualified Data.List as L
import qualified Data.List.Split as LS
import GHC.Base (undefined)

-- |Tensor error, returned by functions that might fail depending on the shape of its argument(s).
newtype Error = Error String deriving(Show)

data Interval = Interval Int Int

instance Show Interval where
    show (Interval lower upper) = show lower ++ ":" ++ show upper

data Zero
data Succ n
data SNat n where
    SZero :: SNat Zero
    SSucc :: SNat n -> SNat (Succ n)

-- | A list of type a and of length n
data ListN a n where
    Nil  :: ListN a Zero
    Cons :: a -> ListN a n -> ListN a (Succ n)

instance (Show a) => Show (ListN a n) where
    show Nil = "Nil"
    show (Cons x xs@Nil) = "Cons " ++ show x ++ " " ++ show xs
    show (Cons x xs) = "Cons " ++ show x  ++ " (" ++ show xs ++ ")"

data ListAnyN a where
    ListAnyN :: SNat n -> ListN a n -> ListAnyN a

instance (Show a) => Show (ListAnyN a) where
    show (ListAnyN sn ln) = "ListAnyN (" ++ show ln ++ ")"

listToListN :: [a] -> ListAnyN a
listToListN []      = ListAnyN SZero Nil
listToListN (x:xs)  =
    case listToListN xs of
        ListAnyN sn ln -> ListAnyN (SSucc sn) (Cons x ln)

-- |Function to create a Interval. Fails if incorrect values are provided.
(|:|) :: Int        -- ^Inclusive lower bound of Interval. Needs to be @0 <= lower bound@
        -> Int      -- ^Exclusive upper bound of Interval. Needs to be @lower bound< upper bound@
        -> Interval    -- ^Returns a Interval on succes, fails if the requirements are not satisfied.
lower |:| upper | lower >= 0 && upper > lower = Interval lower upper
                | otherwise = error $ "Invalid Interval with parameters " ++ show lower ++ " and " ++ show upper

inInterval :: Interval -> Int -> Bool
inInterval (Interval lower upper) n = lower <= n && n < upper

-- |Main Tensor datatype.
data Tensor a = Dense [a] [Int] | Sparse

-- |Returns the shape of the `Tensor`.
shape :: Tensor a -- ^The `Tensor` we want to know the shape of.
        -> [Int] -- ^Returns the shape of the argument Tensor.
shape (Dense _ s) = s
shape Sparse = undefined

-- |Returns the values of the `Tensor`.
values :: Tensor a  -- ^The `Tensor` we want to know the values of.
        -> [a] -- ^Returns the values of the argument Tensor.
values (Dense v _) = v
values Sparse = undefined

instance (Show a) => Show (Tensor a) where
    show (Dense xs []) = show xs
    show (Dense xs (s:shape)) = "[" ++ L.intercalate ", " (map (\x -> show $ let (Right vec) = fromList x shape in vec) (LS.chunksOf (length xs  `div` s) xs)) ++ "]"
    show Sparse = undefined

-- |Reshaping Tensors.
reshape :: Tensor a  -- ^`Tensor` to be reshaped.
        -> [Int] -- ^The new shape. Its product should equal the product of the old shape.
        -> Either Error (Tensor a) -- ^Returns the `Tensor` with the new shape on succes.
reshape (Dense d shape) newshape    | product shape == product newshape = Right $ Dense d newshape
                                    | otherwise = Left $ Error $ "Cannot reshape Tensor of shape " ++ show shape ++ " to new shape " ++ show newshape ++ "."
reshape Sparse newshape = undefined


-- |Creates a vector from a given list.
vector :: [a] -- ^The list containing the elements of the vector.
        -> Tensor a -- ^Returns a 1D `Tensor`.
vector xs = Dense xs [length xs]

-- |Creates a `Tensor` of the given shape from a list of elements.
fromList :: [a] -- ^A flat list containing the elements of the tensor.
        -> [Int] -- ^The shape of the tensor, such that @product shape = length list@
        -> Either Error (Tensor a) -- ^Returns a `Tensor` of the given shape holding the data on success.
fromList as shape   | product shape == length as = Right $ Dense as shape
                    | otherwise = Left $ Error $ "List with " ++ show (length as) ++ " elements cannot be casted to a square Tensor of shape " ++ show shape ++ "."

-- |Indexing Tensors. Retrieves a single element at the specified index.
--
-- __Example__
--
-- >>> [[1, 2, 3], [4, 5, 6]] |@| [1, 0]
-- 4
--
-- >>> [5, 9, 6, 7] |@| [2]
-- 6

-- | toIndex :: size -> indices -> current dimension level -> index
toIndex :: ListN Int (Succ n) -> ListN Int (Succ n) -> Int -> Int
toIndex (Cons x Nil) (Cons y Nil) d = d * y
toIndex (Cons x xs@(Cons _ _)) (Cons y ys@(Cons _ _)) d = d * y + toIndex xs ys (d * x)

getIndex :: Tensor a
        -> [Int] -- ^List of indices, one integer for each dimension.
        -> Either Error a -- ^Returns an element of the Tensor on succes.
-- This assumes that there is no `Tensor` with shape [], which should be enforced by the functions used to create tensors.
getIndex (Dense xs []) index = undefined
getIndex (Dense xs s@(_:shape)) index = let idx = sum [a * b |(a, b) <- zip (shape ++ [1]) index]
                            in  if idx < length xs
                                then Right $ xs !! idx
                                else Left $ Error $ "Tensor index " ++ show index ++ " is out of bounds for Tensor of shape " ++ show s
getIndex Sparse _ = undefined

-- |Slicing allows for taking a contiguous subsection of a `Tensor`.
--
-- __Examples__
-- 
-- Taking a 2x2 subtensor of a 3x3 tensor by taking elements [0, 2) along
-- the first dimension, and elements [1, 3) along the second dimension.
--
-- >>> [[1, 2, 3], [4, 5, 6], [7, 8, 9]] |#| [0|:|2, 1|:|3]
-- [[2, 3], [5, 6]]
-- 
-- Taking only the first row and all elements along the second dimension results in the first row vector
-- of the matrix:
--
-- >>> [[1, 2, 3], [4, 5, 6], [7, 8, 9]] |#| [0|:|1, 0|:|3]
-- [[1, 2, 3]]
--
-- Note that the dimensionality does not change, e.g. it will have shape @[1, 3]@
-- 
-- Similarly, we can take the first two rows instead:
--
-- >>> [[1, 2, 3], [4, 5, 6], [7, 8, 9]] |#| [0|:|2, 0|:|3]
-- [[1, 2, 3], [4, 5, 6]]
-- 

(|#|) :: Tensor a -- ^The tensor to take the subtensor of.
        -> [Interval] -- ^List of intervals for each dimension to be included in the new tensor.
        -> Either Error (Tensor a) -- ^Returns a new `Tensor` of the same dimensionality on succes.
(Dense d shape) |#| intervals   | validInterval shape intervals = let
                                        mods = shape
                                        divs = tail shape ++ [1]
                                        flatToNested = zipWith (curry (\ (m, d) x -> x `div` d `mod` m)) mods divs
                                        flatIdx = [0..length d-1]
                                        nested = map (\i -> map ($i) flatToNested) flatIdx -- [[dim0, dim1, dim2], [dim0, dim1, dim2]]
                                        nestedData = zip nested d
                                        filtered = filter (\(n, x) -> all (\(idx, inter) -> inInterval inter idx) (zip n intervals)) nestedData
                                        new_data = [x | (_, x) <- filtered]
                                        new_shape = map (\(Interval lower upper) -> upper - lower) intervals
                                    in Right $ Dense new_data new_shape
                                | otherwise = Left $ Error $ "Intervals " ++ show intervals ++ " are not valid for a Tensor of shape " ++ show shape ++ "."
Sparse |#| _ = undefined

-- Helper function, checks whether a given Interval is valid for a shape.
validInterval :: [Int] -> [Interval] -> Bool
validInterval shape interval  | length shape == length interval = and [u <= x|(x, Interval _ u) <- zip shape interval]
                        | otherwise = False

-- |Indexing Tensors. Retrieves a single element at the specified index.
(|@|) :: Tensor a
        -> [Int] -- ^List of indices, one integer for each dimension.
        -> Either Error a -- ^Returns an element of the Tensor on succes.
Sparse |@| _ = undefined
(Dense vals []) |@| indices = undefined
(Dense vals shape@(_:s)) |@| indices | correctIndex indices shape = Right $ vals !! multiDimensionIndexToOne (s ++ [1]) indices
                                    | otherwise = Left $ Error $ "Dimensions " ++ show indices ++ " do not fit into dimensions " ++ show shape ++ "."
                          where multiDimensionIndexToOne (x:xs) (i:is) = foldr (*) (x * i) xs + multiDimensionIndexToOne xs is
                                multiDimensionIndexToOne [] [] = 0
                                multiDimensionIndexToOne xs [] = undefined
                                multiDimensionIndexToOne [] xs = undefined
                                -- No further pattern matching required due to correctIndex already checking for the length.

correctIndex :: [Int] -> [Int] -> Bool
correctIndex [] [] = True -- If both are empty the indices are correct
correctIndex (x:xs) (y:ys) = x <= y && correctIndex xs ys -- Neither are emtpy, therefore must be evaluated
correctIndex _ _ = False -- One is empty, one is not, length is not the same, therefore not correct

mapTensor :: (a -> b) -> Tensor a -> Tensor b
mapTensor f (Dense v s) = Dense (map f v) s
mapTensor _ Sparse = undefined

-- zipWithTensor :: (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
-- zipWithTensor f (Dense v1 s1) (Dense v2 s2) = Dense (zipWith f v1 v2) s
-- zipWithTensor f Sparse _ = undefined
-- zipWithTensor f _ Sparse = undefined 
