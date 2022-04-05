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

lengthListN :: ListN n a -> Nat
lengthListN Nil = Zero
lengthListN (Cons x xs) = Succ (lengthListN xs)

instance (Show a) => Show (ListN n a) where
    show Nil = "Nil"
    show (Cons x xs@Nil) = "Cons " ++ show x ++ " " ++ show xs
    show (Cons x xs) = "Cons " ++ show x  ++ " (" ++ show xs ++ ")"

instance Foldable (ListN a) where
    foldr f b Nil = b
    foldr f b (Cons x xs) = f x (foldr f b xs)


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

-- |Returns the shape of the `Tensor`.
shape :: Tensor n a-- ^The `Tensor` we want to know the shape of.
        -> [Nat] -- ^Returns the shape of the argument Tensor.
shape (Dense _ :: Tensor l m) = undefined
shape Sparse = undefined

instance (Show a) => Show (Tensor n a) where
    show (Dense ls) = "Dense (" ++ show ls ++ ")"
    --show (Dense xs (s:shape)) = "[" ++ L.intercalate ", " (map (\x -> show $ let (Right vec) = fromList x shape in vec) (LS.chunksOf (length xs  `div` s) xs)) ++ "]"
    show Sparse = undefined

reshape :: (Product ls1 ~ l1, Product ls2 ~ l2, l1 ~ l2 ) =>
        Tensor ls1 a -- ^`Tensor` to be reshaped. 
        -> SameProduct ls1 ls2 -- ^The new shape. Its product should equal the product of the old shape.
        -> Tensor ls2 a -- ^Returns the `Tensor` with the new shape on succes.
reshape (Dense shape) _ = Dense shape
reshape Sparse _ = undefined


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