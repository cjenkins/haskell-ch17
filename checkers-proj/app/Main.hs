module Main where

import Lib

import Control.Applicative
import Data.Monoid
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull =
  Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq

xs :: [(String, String, Int)]
xs = [("b", "w", 1)]

--List Applicative
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons aTob laTob) (Cons a la) = Cons (aTob a) (pure aTob <*> la)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    frequency [ (1, return Nil)
              , (3, return (Cons a1 a2))]

instance Eq a => EqProp (List a) where (=-=) = eq

myList :: List (String, String, Int)
myList = Cons ("b", "w", 1) Nil

--Ziplist
{-
take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' i (Cons h t) = Cons h (take' (i - 1) t)
-}

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take 30 l
          ys' = let (ZipList' l) = ys in take 30 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat a
  (<*>) (ZipList' []) _ = ZipList' []
  (<*>) _ (ZipList' []) = ZipList' []
  (<*>) (ZipList' zlaTob) (ZipList' zla) = ZipList' $ zipWith ($) zlaTob zla

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return (ZipList' a)

myZipList :: ZipList' (String, String, Int)
myZipList = ZipList' [("b", "w", 1)]

--Validation
data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  (<*>) (Failure e1) (Failure e2) = Failure $ e1 <> e2
  (<*>) (Failure e) _ = Failure e
  (<*>) _ (Failure e) = Failure e
  (<*>) (Success aTob) (Success a) = Success (aTob a)

instance (Eq a, Eq e) => EqProp (Validation e a) where (=-=) = eq

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation a e) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    frequency [(1, return $ Failure e), (1, return $ Success a)]

myValidator :: Validation String (String, String, Int)
myValidator = Success ("OK", "Good", 0)

main :: IO ()
main = do
  quickBatch (monoid Twoo)
  quickBatch (applicative xs)
  quickBatch (applicative myList)
  quickBatch (applicative myZipList)
  quickBatch (applicative myValidator)
