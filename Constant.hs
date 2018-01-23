module Constant where

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant b) = Constant b

instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant a1) (Constant a2) = Constant (mappend a1 a2)
