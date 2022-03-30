{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
For using in GHCI you have to use:
:set -XDataKinds
:set -XGADTs
:set -XNoStarIsType
:set -XPolyKinds
:set -XTypeFamilies
:set -XTypeOperators
:set -XUndecidableInstances
:set -XScopedTypeVariables
-}

{-|
Module      : Core
Description : This module describes the core data types and functions of NumHS.
-}
module Core where

import qualified Data.List as L
import qualified Data.List.Split as LS
import GHC.Base (undefined)

-- |Tensor error, returned by functions that might fail depending on the shape of its argument(s).
newtype Error = Error String deriving(Show)

data Interval = Between Nat Nat | Or Interval Interval

instance Show Interval where
    show (Between lower upper) = show lower ++ ":" ++ show upper
    show (Or left right) = "(" ++ show left ++ ") | (" ++ show right ++ ")"

data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat
    deriving Show

instance Eq Nat where
    Zero == Zero = True
    Zero == Succ x = False
    Succ x == Zero = False
    Succ x == Succ y = x == y

instance Ord Nat where
    Zero <= _ = True 
    Succ x <= Zero = False
    Succ x <= Succ y = x <= y


-- | A list of type a and of length n
data ListN (dim :: Nat) a where
    Nil  :: ListN Zero a
    Cons :: a -> ListN n a -> ListN (Succ n) a
infixr 5 `Cons`

instance (Show a) => Show (ListN n a) where
    show Nil = "Nil"
    show (Cons x xs@Nil) = "Cons " ++ show x ++ " " ++ show xs
    show (Cons x xs) = "Cons " ++ show x  ++ " (" ++ show xs ++ ")"

instance Foldable (ListN a) where
    foldr f b Nil = b
    foldr f b (Cons x xs) = f x (foldr f b xs)

fromInt :: (Num t, Ord t) => t -> Nat
fromInt 0   = Zero
fromInt x   | x > 0 = Succ (fromInt $ x - 1)
            | otherwise = error "fromInt doesn't work with negative numbers"

-- |Function to create a Interval. Fails if incorrect values are provided.
(|:|) :: Nat       -- ^Inclusive lower bound of Interval. Needs to be @0 <= lower bound@
        -> Nat       -- ^Exclusive upper bound of Interval. Needs to be @lower bound< upper bound@
        -> Interval    -- ^Returns a Interval on succes, fails if the requirements are not satisfied.
lower |:| upper | lower >= Zero && upper > lower = Between lower upper
                | otherwise = error $ "Invalid Interval with parameters " ++ show lower ++ " and " ++ show upper

inInterval :: Interval -> Nat -> Bool
inInterval (Between lower upper) n = lower <= n && n < upper
inInterval (Or left right) n = inInterval left n || inInterval right n

type family Plus m n where
    Plus (Succ m) n = Plus m (Succ n)
    Plus Zero n = n

type family Times m n where
    Times (Succ m) n = Plus n (Times m n)
    Times Zero n = Zero

type family Product (dims) where
    Product '[] = Succ Zero
    Product (m : ns) = Times m (Product ns)

type family Length dims where
    Length '[] = Zero
    Length (m : ms) = Succ (Length ms)

data SameProduct (dims1 :: [Nat]) (dims2 :: [Nat]) where
    Same :: (Product dims1 ~ d1, Product dims2 ~ d2, d1 ~ d2) => SameProduct dims1 dims2

-- |Main Tensor datatype.
data Tensor (dims :: [Nat]) a where
    Dense :: (Product dims ~ n) => ListN n a -> Tensor dims a
    Sparse :: Tensor n a

t1 :: Tensor '[Succ Zero, Succ (Succ Zero), Succ (Succ (Succ Zero))] Int
t1 = Dense (1 `Cons` 2 `Cons` 3 `Cons` 4 `Cons` 5 `Cons` 6 `Cons` Nil)

getCompleteDimension :: ListN n Int-> Int
getCompleteDimension Nil = 0
getCompleteDimension (Cons x xs) = x * getCompleteDimension xs

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat x  | x > 0 = Succ (intToNat $ x - 1)
            | otherwise = error "intToNat doesn't work with negative numbers"


-- |Returns the shape of the `Tensor`.
shape :: Tensor n a-- ^The `Tensor` we want to know the shape of.
        -> [Nat] -- ^Returns the shape of the argument Tensor.
shape (Dense _ :: Tensor l m) = undefined
shape Sparse = undefined

instance (Show a) => Show (Tensor n a) where
    show (Dense ls) = "Dense (" ++ show ls ++ ")"
    --show (Dense xs (s:shape)) = "[" ++ L.intercalate ", " (map (\x -> show $ let (Right vec) = fromList x shape in vec) (LS.chunksOf (length xs  `div` s) xs)) ++ "]"
    show Sparse = undefined

(!!) :: ListN n a -> Nat -> a
Nil !! _ = error "There is no element left to get."
(Cons x xs) !! Zero = x
(Cons x xs) !! (Succ n) = xs Core.!! n

-- |Reshaping Tensors.
reshape :: (Product ls1 ~ l1, Product ls2 ~ l2, l1 ~ l2 ) =>
        Tensor ls1 a -- ^`Tensor` to be reshaped. 
        -> SameProduct ls1 ls2 -- ^The new shape. Its product should equal the product of the old shape.
        -> Tensor ls2 a -- ^Returns the `Tensor` with the new shape on succes.
reshape (Dense shape) _ = Dense shape
reshape Sparse _ = undefined

lengthListN :: ListN n a -> Nat
lengthListN Nil = Zero
lengthListN (Cons x xs) = Succ (lengthListN xs)

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ x) = 1 + natToInt x


-- |Creates a vector from a given list.
vector :: (Product l ~ m, Length l ~ Succ Zero) =>
        ListN m a -- ^The list containing the elements of the vector.
        -> Tensor l a -- ^Returns a 1D `Tensor`.
vector Nil = Dense Nil
vector l@(Cons _ _) = Dense l

-- |Creates a `Tensor` of the given shape from a list of elements.
fromListWithSP :: (Product l1 ~ l, Product l2 ~ n, l ~ n ) =>
        ListN n a -- ^A flat list containing the elements of the tensor.
        -> SameProduct l1 l2 -- ^The shape of the tensor, such that @product shape = length list@
        -> Tensor l2 a -- ^Returns a `Tensor` of the given shape holding the data on success.
fromListWithSP as _   = Dense as

fromList :: (Product l1 ~ n, l ~ n ) =>
        ListN n a -- ^A flat list containing the elements of the tensor.
        -> Tensor l1 a -- ^Returns a `Tensor` of the given shape holding the data on success.
fromList = Dense

-- |Indexing Tensors. Retrieves a single element at the specified index.
--
-- __Example__
--
-- >>> [[1, 2, 3], [4, 5, 6]] |@| [1, 0]
-- 4
--
-- >>> [5, 9, 6, 7] |@| [2]
-- 6

-- toIndexHelper :: ListN (Succ n) Int -> ListN (Succ n) Int -> Int -> Either Error Int
-- toIndexHelper (Cons Nil x) (Cons Nil y) d   | y < x = Right (d * y)
--                                             | otherwise = Left $ Error $ "coordinate " ++ show y ++ " is too large for dimension " ++ show x
-- toIndexHelper (Cons xs@(Cons _ _) x) (Cons ys@(Cons _ _) y) d
--     | y < x =
--         case toIndexHelper xs ys (d * x) of
--             Left msg -> Left msg
--             Right rest -> Right (d * y + rest)
--     | otherwise = Left $ Error $ "coordinate " ++ show y ++ " is too large for dimension " ++ show x

-- -- | toIndex :: size -> indices -> current dimension level -> index
-- toIndex :: ListN (Succ n) Int -> ListN (Succ n) Int -> Either Error Nat
-- toIndex x y =
--     case toIndexHelper x y 1 of
--         Left msg -> Left msg
--         Right x -> Right (fromInt x)

(|@|) :: Tensor n a
        -> [Nat] -- ^List of indices, one integer for each dimension.
        -> Either Error a -- ^Returns an element of the Tensor on succes.
_ |@| _ = undefined
-- This assumes that there is no `Tensor` with shape [], which should be enforced by the functions used to create tensors.
-- (Dense Nil) |@| _ = Left (Error "The tensor doesn't contain any elements.")
-- (Dense xs Nil) |@| _ = Left (Error "The tensor doesn't have any dimension.")
-- (Dense xs shape@(Cons _ _)) |@| index@(Cons _ _) = case toIndex shape index of
--     Left msg -> Left msg
--     Right n -> Right $ xs Core.!! n
-- Sparse |@| _ = undefined

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

-- helperFilter :: ListN n a -> ListN n Interval -> SNat m -> ListN m a
-- helperFilter Nil _ = Nil

(|#|) :: (Product n ~ m) => Tensor n a -> ListN m Interval -> Either Error (Tensor q a)
(|#|) = undefined



-- (|#|) :: Tensor m n a-- ^The tensor to take the subtensor of.
--         -> ListN n Interval -- ^List of intervals for each dimension to be included in the new tensor.
--         -> Either Error (Tensor p q a) -- ^Returns a new `Tensor` of the same dimensionality on succes.
-- (Dense d shape) |#| intervals   | validInterval shape intervals = let
--                                         mods = shape
--                                         divs = tail shape ++ [1]
--                                         flatToNested = zipWith (curry (\ (m, d) x -> x `div` d `mod` m)) mods divs
--                                         flatIdx = [0..length d-1]
--                                         nested = map (\i -> map ($i) flatToNested) flatIdx -- [[dim0, dim1, dim2], [dim0, dim1, dim2]]
--                                         nestedData = zip nested d
--                                         filtered = filter (\(n, x) -> all (\(idx, inter) -> inInterval inter idx) (zip n intervals)) nestedData
--                                         new_data = [x | (_, x) <- filtered]
--                                         new_shape = map (\(Interval lower upper) -> upper - lower) intervals
--                                     in Right $ Dense new_data new_shape
--                                 | otherwise = Left $ Error $ "Intervals " ++ show intervals ++ " are not valid for a Tensor of shape " ++ show shape ++ "."
-- Sparse |#| _ = undefined

-- -- Helper function, checks whether a given Interval is valid for a shape.
-- validInterval :: [Int] -> [Interval] -> Bool
-- validInterval shape interval  | length shape == length interval = and [u <= x|(x, Interval _ u) <- zip shape interval]
--                         | otherwise = False

-- IS THIS STILL NECESSARY? I CHANGED THE |@| FUNCTION TO USE THE LISTN DATA TYPE
-- -- |Indexing Tensors. Retrieves a single element at the specified index.
-- getIndex :: Tensor m n a
--         -> [Int] -- ^List of indices, one integer for each dimension.
--         -> Either Error a -- ^Returns an element of the Tensor on succes.
-- getIndex Sparse _ = undefined
-- getIndex (Dense vals []) indices = undefined
-- getIndex (Dense vals shape@(_:s)) indices | correctIndex shape indices = Right $ vals Core.!! multiDimensionIndexToOne (s ++ [1]) indices
--                                     | otherwise = Left $ Error $ "Dimensions " ++ show indices ++ " do not fit into dimensions " ++ show shape ++ "."
--                           where multiDimensionIndexToOne (x:xs) (i:is) = foldr (*) (x * i) xs + multiDimensionIndexToOne xs is
--                                 multiDimensionIndexToOne [] [] = 0
--                                 multiDimensionIndexToOne xs [] = undefined
--                                 multiDimensionIndexToOne [] xs = undefined
--                                 -- No further pattern matching required due to correctIndex already checking for the length.

correctIndex :: [Int] -> [Int] -> Bool
correctIndex [] [] = True -- If both are empty the indices are correct
correctIndex (x:xs) (y:ys) = x <= y && correctIndex xs ys -- Neither are emtpy, therefore must be evaluated
correctIndex _ _ = False -- One is empty, one is not, length is not the same, therefore not correct
