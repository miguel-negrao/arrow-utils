{-# LANGUAGE ScopedTypeVariables #-}

-- test-framework
import Test.Framework ( testGroup, defaultMain, Test )

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2 ( testProperty )

-- QuickCheck
import Test.QuickCheck ( (===) )

import Control.Arrow.Utils ( zipSequenceArrList )

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests = [
  testGroup "sequenceArrList"
    [ testProperty "length of arrows"
      $ \(xs :: [Integer]) (ys :: [Integer])-> let
         funcList = fmap (const (+1)) ys
         res = zipSequenceArrList funcList xs
        in
          length res === min (length xs) (length ys)                           
    ]
  ]

