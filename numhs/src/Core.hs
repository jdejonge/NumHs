{-|
Module      : Core
Description : This module describes the core data types and functions of NumHS.
-}
module Core(
    vector,
    fromList,
    shape,
    (#)
) where

import qualified Data.List as L
import qualified Data.List.Split as LS

data Error = Error String deriving(Show)


data Tensor a = Dense [a] [Int] | Sparse

-- |Returns the shape of the Tensor.
shape :: Tensor a -- ^The Tensor we want to know the shape of.
        -> [Int] -- ^Returns the shape of the argument Tensor.
shape (Dense _ s) = s
shape Sparse = undefined

instance (Show a) => Show (Tensor a) where
    show (Dense xs [_]) = show xs
    show (Dense xs (s:shape)) = "[" ++ L.intercalate ", " (map (\x -> show $ let (Right vec) = fromList x shape in vec) (LS.chunksOf (length xs  `div` s) xs)) ++ "]"
    show Sparse = undefined

-- |Creates a vector from a given list.
vector :: [a] -- ^The list containing the elements of the vector.
        -> Tensor a -- ^Returns a 1D `Tensor`.
vector xs = Dense xs $ [length xs]

-- |Creates a `Tensor` of the given shape from a list of elements.
fromList :: [a] -- ^A flat list containing the elements of the tensor.
        -> [Int] -- ^The shape of the tensor, such that @product shape = length list@
        -> Either Error (Tensor a) -- ^Returns a `Tensor` of the given shape holding the data on success.
fromList as shape   | product shape == length as = Right $ Dense as shape
                    | otherwise = Left $ Error $ "List with " ++ show (length as) ++ " elements cannot be casted to a square Tensor of shape " ++ show shape ++ "."

-- |Indexing Tensors. Retrieves a single element at the specified index.
(#) :: Tensor a 
        -> [Int] -- ^List of indices, one integer for each dimension.
        -> Either Error a -- ^Returns an element of the Tensor on succes.

-- This assumes that there is no Tensor with shape [], which should be enforced by the functions used to create tensors.
(Dense xs s@(_:shape)) # index = let idx = sum [a * b |(a, b) <- zip (shape ++ [1]) index]
                            in case idx < length xs of 
                                True -> Right $ xs !! idx
                                False -> Left $ Error $ "Tensor index " ++ show index ++ " is out of bounds for Tensor of shape " ++ show s
Sparse # _ = undefined
