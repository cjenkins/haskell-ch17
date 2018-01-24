data Three a b c = Three a b c deriving (Eq, Show)
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three a1 b1 f) (Three a2 b2 c)= Three (a1 <> a2) (b1 <> b2) (f c)

instance Functor (Three' a) where
  fmap f (Three' a b b) = Three' a (f b) (f b)

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a1 f1 f2) (Three' a2 b1 b2)= Three' (a1 <> a2) (f1 b1) (f2 b2)
