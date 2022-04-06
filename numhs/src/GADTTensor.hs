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

module GADTTensor where


data NatG where
    ZeroG :: NatG
    SuccG :: NatG -> NatG
    deriving Show

instance Eq NatG where
    ZeroG == ZeroG = True
    ZeroG == SuccG x = False
    SuccG x == ZeroG = False
    SuccG x == SuccG y = x == y

instance Ord NatG where
    ZeroG <= _ = True 
    SuccG x <= ZeroG = False
    SuccG x <= SuccG y = x <= y


-- | A list of type a and of length n
data ListNG (dim :: NatG) a where
    NilG  :: ListNG ZeroG a
    ConsG :: a -> ListNG n a -> ListNG (SuccG n) a
infixr 5 `ConsG`

lengthListN :: ListNG n a -> NatG
lengthListN NilG = ZeroG
lengthListN (ConsG x xs) = SuccG (lengthListN xs)

instance (Show a) => Show (ListNG n a) where
    show NilG = "NilG"
    show (ConsG x xs@NilG) = "ConsG " ++ show x ++ " " ++ show xs
    show (ConsG x xs) = "ConsG " ++ show x  ++ " (" ++ show xs ++ ")"

instance Functor (ListNG n) where
    fmap f NilG = NilG
    fmap f (ConsG x xs) = ConsG (f x) (fmap f xs)

instance Foldable (ListNG n) where
    foldr f b NilG = b
    foldr f b (ConsG x xs) = f x (foldr f b xs)


apply :: ListNG n (a -> b) -> ListNG n a -> ListNG n b
apply NilG NilG = NilG
apply (ConsG f fs) (ConsG x xs) = ConsG (f x) (apply fs xs)

-- getIndex :: (Less m n ~ True) => ListNG n a -> m -> a
-- getIndex (ConsG x xs) ZeroG = x
-- getIndex (ConsG x xs) (SuccG n) = getIndex xs n

type family Plus m n where
    Plus (SuccG m) n = Plus m (SuccG n)
    Plus ZeroG n = n

type family Times m n where
    Times (SuccG m) n = Plus n (Times m n)
    Times ZeroG n = ZeroG

type family Product (dims) where
    Product '[] = SuccG ZeroG
    Product (m : ns) = Times m (Product ns)

type family Length dims where
    Length '[] = ZeroG
    Length (m : ms) = SuccG (Length ms)

type family Less n1 n2 where
    Less ZeroG (SuccG n) = True
    Less (SuccG n) (SuccG m) = Less n m

data SameProduct (dims1 :: [NatG]) (dims2 :: [NatG]) where
    Same :: (Product dims1 ~ d1, Product dims2 ~ d2, d1 ~ d2) => SameProduct dims1 dims2

-- |Main Tensor datatype.
data TensorG (dims :: [NatG]) a where
    DenseG :: (Product dims ~ n) => ListNG n a -> TensorG dims a
    SparseG :: TensorG n a

-- |Returns the shape of the `Tensor`.
shape :: TensorG n a-- ^The `Tensor` we want to know the shape of.
        -> [NatG] -- ^Returns the shape of the argument Tensor.
shape (DenseG _ :: TensorG l m) = undefined
shape SparseG = undefined

instance (Show a) => Show (TensorG n a) where
    show (DenseG ls) = "DenseG (" ++ show ls ++ ")"
    --show (Dense xs (s:shape)) = "[" ++ L.intercalate ", " (map (\x -> show $ let (Right vec) = fromList x shape in vec) (LS.chunksOf (length xs  `div` s) xs)) ++ "]"
    show SparseG = undefined

reshapeGWithSP :: (Product ls1 ~ Product ls2) =>
        TensorG ls1 a -- ^`Tensor` to be reshaped. 
        -> SameProduct ls1 ls2 -- ^The new shape. Its product should equal the product of the old shape.
        -> TensorG ls2 a -- ^Returns the `Tensor` with the new shape on succes.
reshapeGWithSP (DenseG ls) _ = DenseG ls
reshapeGWithSP SparseG _ = undefined

reshapeG :: (Product ls1 ~ Product ls2) =>
        TensorG ls1 a -- ^`Tensor` to be reshaped. 
        -> TensorG ls2 a -- ^Returns the `Tensor` with the new shape on succes.
reshapeG (DenseG ls) = DenseG ls
reshapeG SparseG = SparseG


-- |Creates a vector from a given list.
vectorG :: (Product l ~ m, Length l ~ SuccG ZeroG) =>
        ListNG m a -- ^The list containing the elements of the vector.
        -> TensorG l a -- ^Returns a 1D `Tensor`.
vectorG NilG = DenseG NilG
vectorG l@(ConsG _ _) = DenseG l

-- |Creates a `Tensor` of the given shape from a list of elements.
fromListGWithSP :: (Product l1 ~ l, Product l2 ~ n, l ~ n ) =>
        ListNG n a -- ^A flat list containing the elements of the tensor.
        -> SameProduct l1 l2 -- ^The shape of the tensor, such that @product shape = length list@
        -> TensorG l2 a -- ^Returns a `Tensor` of the given shape holding the data on success.
fromListGWithSP as _   = DenseG as

fromListG :: (Product l1 ~ n) =>
        ListNG n a -- ^A flat list containing the elements of the tensor.
        -> TensorG l1 a -- ^Returns a `Tensor` of the given shape holding the data on success.
fromListG = DenseG

addG :: (Num a, l1 ~ l2) =>
    TensorG l1 a
    -> TensorG l2 a
    -> TensorG l2 a
addG (DenseG ls1) (DenseG ls2) = DenseG (apply (fmap (+) ls1) ls2)
addG _ _ = undefined

multiplyPointWise :: (Num a, l1 ~ l2) =>
    TensorG l1 a
    -> TensorG l2 a
    -> TensorG l2 a
multiplyPointWise (DenseG ls1) (DenseG ls2) = DenseG (apply (fmap (*) ls1) ls2)
multiplyPointWise _ _ = undefined

t1 :: TensorG '[SuccG ZeroG, SuccG (SuccG ZeroG), SuccG (SuccG (SuccG ZeroG))] Int
t1 = DenseG (1 `ConsG` 2 `ConsG` 3 `ConsG` 4 `ConsG` 5 `ConsG` 6 `ConsG` NilG)

t2 :: TensorG '[SuccG ZeroG, SuccG (SuccG ZeroG), SuccG (SuccG (SuccG ZeroG))] Int
t2 = DenseG (10 `ConsG` 20 `ConsG` 30 `ConsG` 40 `ConsG` 50 `ConsG` 60 `ConsG` NilG)

t3 :: TensorG '[SuccG (SuccG ZeroG), SuccG ZeroG, SuccG (SuccG (SuccG ZeroG))] Int
t3 = DenseG (10 `ConsG` 20 `ConsG` 30 `ConsG` 40 `ConsG` 50 `ConsG` 60 `ConsG` NilG)

-- getElementBy1DIndex :: (Product ) => TensorG n a -> m -> a
-- getElementBy1DIndex (DenseG ls) = get