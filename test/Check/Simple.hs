{-# LANGUAGE TemplateHaskell #-}

module Check.Simple where

import           Data.Maybe
import           Hedgehog
import           Hedgehog.Gen   as Gen
import           Hedgehog.Range as Range
import qualified Koan.Simple    as K
import           Prelude        as P

prop_id :: Property
prop_id = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  K.id a === P.id a

prop_const :: Property
prop_const = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  b <- forAll $ Gen.int (Range.constantBounded)
  K.const a b === P.const a b

prop_compose_op :: Property
prop_compose_op = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  ((+1) K.. (+2)) a === ((+1) . (+2)) a
  ((*2) K.. (+3)) a === ((*2) . (+3)) a

prop_flip :: Property
prop_flip = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  b <- forAll $ Gen.int (Range.constantBounded)
  (K.flip (+)) a b === (P.flip (+)) a b
  (K.flip (-)) a b === (P.flip (-)) a b

prop_apply_op :: Property
prop_apply_op = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  b <- forAll $ Gen.int (Range.constantBounded)
  ((+a) K.$ b) === ((+a) P.$ b)

prop_length :: Property
prop_length = property $ do
  as <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.constantBounded))
  K.length as === P.length as

prop_index_op :: Property
prop_index_op = property $ do
  as <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.constantBounded))
  i  <- forAll $ Gen.int (Range.linear 0 (P.length as - 1))
  as K.!! i === listToMaybe (drop i as)

prop_concat_op :: Property
prop_concat_op = property $ do
  as <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.constantBounded))
  bs <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.constantBounded))
  as K.++ bs === as P.++ bs

prop_reverse :: Property
prop_reverse = property $ do
  as <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.constantBounded))
  K.reverse as === P.reverse as

-- --------------------------------------------------------------------------------
-- -- Infinite lists
-- --------------------------------------------------------------------------------

prop_repeat :: Property
prop_repeat = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  take 100 (K.repeat a) === take 100 (P.repeat a)

prop_iterate :: Property
prop_iterate = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  take 100 (K.iterate (+1) a) === take 100 (P.iterate (+1) a)

prop_take :: Property
prop_take = property $ do
  as <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.constantBounded))
  i  <- forAll $ Gen.int (Range.linear 0 (P.length as - 1))
  K.take i as === P.take i as

prop_drop :: Property
prop_drop = property $ do
  as <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  i  <- forAll $ Gen.int (Range.linear 0 (P.length as))
  K.drop i as === P.drop i as

prop_takeWhile :: Property
prop_takeWhile = property $ do
  as <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.takeWhile even as === P.takeWhile even as

prop_dropWhile :: Property
prop_dropWhile = property $ do
  as <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.dropWhile even as === P.dropWhile even as

prop_map :: Property
prop_map = property $ do
  as <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.map (+1) as === P.map (+1) as

prop_filter :: Property
prop_filter = property $ do
  as <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.filter even as === P.filter even as

prop_foldl :: Property
prop_foldl = property $ do
  as <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.foldl (+) 0 as === P.foldl (+) 0 as

prop_foldr :: Property
prop_foldr = property $ do
  as <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.foldr (+) 0 as === P.foldr (+) 0 as

prop_any :: Property
prop_any = property $ do
  as <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.any even as === P.any even as

prop_all :: Property
prop_all = property $ do
  as <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.all even as === P.all even as

prop_zipWith :: Property
prop_zipWith = property $ do
  as <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  bs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.zipWith (+) as bs === P.zipWith (+) as bs

prop_curryAndUncurry :: Property
prop_curryAndUncurry = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  b <- forAll $ Gen.int (Range.constantBounded)
  P.curry (K.uncurry (+)) a b === a + b
  K.curry (P.uncurry (+)) a b === a + b

prop_elem :: Property
prop_elem = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  as <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.elem a as === P.elem a as

prop_max :: Property
prop_max = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  b <- forAll $ Gen.int (Range.constantBounded)
  K.max a b === P.max a b

prop_min :: Property
prop_min = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  b <- forAll $ Gen.int (Range.constantBounded)
  K.min a b === P.min a b

prop_maximum :: Property
prop_maximum = property $ do
  as <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.constantBounded))
  K.maximum as === P.maximum as

prop_minimum :: Property
prop_minimum = property $ do
  as <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.constantBounded))
  K.minimum as === P.minimum as

prop_fibonacci :: Property
prop_fibonacci = property $ do
  i <- forAll $ Gen.int (Range.linear 0 100)
  K.fibonacci !! 0 === 1
  K.fibonacci !! 1 === 1
  K.fibonacci !! 2 === 2
  (K.fibonacci !! i) + (K.fibonacci !! (i + 1)) === (K.fibonacci !! (i + 2))

tests :: IO Bool
tests = ($$(checkConcurrent))
