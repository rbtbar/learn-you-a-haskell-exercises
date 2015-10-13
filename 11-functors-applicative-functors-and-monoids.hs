module Functors where
import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show, Eq)

--let test = Value 1 (Empty) in show test;
-- Make the list a Functor
instance Functor List where
  fmap _ Empty = Empty
  fmap f (Value x xs) = Value (f x) (fmap f xs)

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty xs = xs
combineLists (Value x xs) ys = Value x (combineLists xs ys)


-- Make our list a Monoid
instance Monoid (List a) where
  mappend = combineLists
  mempty = Empty

-- Make our list an Applicative
instance Applicative List where
  pure a = Value a Empty
  Empty <*> _ = Empty
  (Value f fs) <*> xs = combineLists (fmap f xs) (fs <*> xs)

-- Make sure that the List obeys the laws for Applicative and Monoid

-- Applicative laws
--pure id <*> v = v                            -- Identity
--pure f <*> pure x = pure (f x)               -- Homomorphism
--u <*> pure y = pure ($ y) <*> u              -- Interchange
--pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
identity :: (Eq (f b), Applicative f) =>
              f b -> Bool
identity v = (pure id <*> v) == v

homomorphism :: Eq b => (a -> b) -> a -> Bool
homomorphism u v = (fromPure u <*> fromPure v) == fromPure (u v)

interchange :: (Eq (f b), Applicative f) =>
                 f (a -> b) -> a -> Bool
interchange u y = (u <*> pure y) == (pure ($ y) <*> u)

composition :: (Eq (f b), Applicative f) =>
                 f (a -> b) -> f (a1 -> a) -> f a1 -> Bool
composition u v w  = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

fromPure :: a -> List a
fromPure = pure

obeysApplicativeLaws :: Bool
obeysApplicativeLaws = (True, True, True, True) == (identity k, homomorphism f x, interchange u y, composition m n o)
  where k = Value 1 (Value 2 Empty)
        f = (*64)
        x = 1
        u = fromPure (*128)
        y = 4
        m = fromPure (*512)
        n = fromPure (*1024)
        o = Value 16 (Value 32 Empty)

-- Monoid laws
--(x <*> y) <*> z = x <*> (y <*> z) -- associativity
--mempty <*> x = x               -- left identity
--x <*> mempty = x               -- right identity

associativity :: (Eq a, Monoid a) => a -> a -> a -> Bool
associativity x y z= ((x `mappend` y) `mappend` z) == (x `mappend` (y `mappend` z))

leftIdentity :: (Eq a, Monoid a) => a -> Bool
leftIdentity x = (mempty `mappend` x) == x

rightIdentity :: (Eq a, Monoid a) => a -> Bool
rightIdentity x = (x `mappend` mempty) == x

obeysMonoidLaws :: Bool
obeysMonoidLaws = (True, True, True) == (associativity x y z, leftIdentity x, rightIdentity x)
    where x = Value 1 (Value 2 Empty)
          y = Value 4 (Value 8 Empty)
          z = Value 16 (Value 32 Empty)

-- Create some lists of numbers of different lengths such as:
twoValueList :: List Integer
twoValueList = Value 10 $ Value 20 Empty

threeValueList :: List Integer
threeValueList = Value 10 $ Value 20 $ Value 40 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo :: Integer -> Integer
plusTwo = (+2)

singleParamResult :: List Integer
singleParamResult = plusTwo <$> twoValueList

-- Use <$> and <*> on the lists with a binary function
minus :: Integer -> Integer -> Integer
minus = (-)

binaryParamResult :: List Integer
binaryParamResult = minus <$> twoValueList <*> threeValueList

-- Create some lists of binary functions
binaryFunctionsList :: List (Integer -> Integer -> Integer)
binaryFunctionsList = Value (+) $ Value (*) $ Value (-) Empty

-- Use <*> on the binary functions list and the number lists
binaryFunctionsResult :: List Integer
binaryFunctionsResult = binaryFunctionsList <*> twoValueList <*> threeValueList
