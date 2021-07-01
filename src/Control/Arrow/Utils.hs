{-# LANGUAGE Arrows #-}

module Control.Arrow.Utils (
    SameInputArrow(..)
  , sequenceArr_
  , sequenceArr
  , sequenceArrVec
  , sequenceArrList
  , whenArr
  , unlessArr
  , constantly
) where

import Control.Arrow
    ( returnA, (>>>), Arrow((***), arr), ArrowChoice )
import Data.Foldable (traverse_)
import Data.Maybe ( fromJust )
import Data.Vector.Sized ( fromList, toList, Vector )
import qualified Data.Vector.Sized as Vec
import GHC.TypeLits ( KnownNat )

-- | Wrap the Arrow in a newtype in order to create new class instances.
--   This is a generalisation of 'ArrowMonad',
--   which is isomorphic to @'SameInputArrow' a () c@.
newtype SameInputArrow a b c = SameInputArrow { unSameInputArrow :: a b c }

-- | @'fmap' f@ postcomposes with @f@
instance (Arrow a) => Functor (SameInputArrow a b) where
  fmap f a = SameInputArrow (unSameInputArrow a >>> arr f)

-- | @'<*>'@ runs the arrows in parallel
instance (Arrow a) => Applicative (SameInputArrow a b) where
  pure c = SameInputArrow $ constantly c
  f <*> a = SameInputArrow $ proc input -> do
    fres <- unSameInputArrow f -< input
    ares <- unSameInputArrow a -< input
    returnA -< fres ares

-- | Like 'sequenceArr', but discard the results.
sequenceArr_ :: (Foldable t, Arrow a) => t (a b any) -> a b ()
sequenceArr_ xs = unSameInputArrow $ traverse_ SameInputArrow xs

-- | Run all arrows in the given 'Traversable', collecting the results.
--
--   @sequenceArr [(+1), (+10)] 1 == [2,11]@
sequenceArr :: (Traversable t, Arrow a) => t (a b c) -> a b (t c)
sequenceArr xs = unSameInputArrow $ traverse SameInputArrow xs

-- | Fans each input from @Vector n b@ to a separate arrow from the given vector.
--
--   @sequenceArrVec (Vec.generate ((+).fromIntegral) :: Vector 5 (Int -> Int)) (Vec.replicate 1 :: Vector 5 Int) == Vector [1,2,3,4,5]@
sequenceArrVec :: (Arrow a, KnownNat n) => Vector n (a b c) -> a (Vector n b) (Vector n c)
sequenceArrVec cells = arr toList >>> sequenceArrListUnsafe (toList cells) >>> arr (fromJust . fromList)

-- Not safe, doesn't check size of the lists.
-- When used in sequenceArrVec it is safe as the size of the
-- vectors are all the same. Not to export.
sequenceArrListUnsafe :: Arrow a => [a b c] -> a [b] [c]
sequenceArrListUnsafe [] = constantly []
sequenceArrListUnsafe (x:xs) = proc (y:ys) -> do
  xres <- x -< y
  xsres <- sequenceArrListUnsafe xs -< ys
  returnA -< (xres:xsres)

-- | Fans each input from @[b]@ to a separate arrow from the given list.
--   The output list has length of the minimum of the input list length and the arrow list length.
--
--  @
--  sequenceArrList [(+1), (+10)] [1,2] == [2,12]
--  sequenceArrList [(+1), (+10)] [1]   == [2]
--  sequenceArrList [(+1)] [1,2,3,4]    == [2]@
sequenceArrList :: (Arrow a, ArrowChoice a) => [a b c] -> a [b] [c]
sequenceArrList [] = constantly []
sequenceArrList (a : as) = proc bs' -> case bs' of
  [] -> returnA -< []
  b : bs -> do
    c <- a -< b
    cs <- sequenceArrList as -< bs
    returnA -< c : cs

-- | Similar to @'when'@ for @'Applicative'@. Relevant for
--   arrows which embeded a Monad.
whenArr :: ArrowChoice a => a b () -> a (Bool, b) ()
whenArr cell = proc (b, input) -> do
  if b
    then cell -< input
    else constantly () -< input

-- | Similar to @'unless'@ for @'Applicative'@. Relevant for
--   arrows which embeded a Monad.
unlessArr :: ArrowChoice a => a b () -> a (Bool, b) ()
unlessArr cell = arr not *** arr id >>> whenArr cell

-- | Always output the given value.
constantly :: Arrow a => b -> a any b
constantly = arr . const
