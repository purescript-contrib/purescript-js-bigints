module Test.Main where

import Prelude hiding (not)

import Data.Array (foldMap)
import Data.Array.NonEmpty (cons')
import Data.Foldable (fold)
import Data.Int (base36)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (un)
import Debug (spy)
import Effect (Effect)
import Effect.Console (log)
import JS.BigInt (BigInt, and, binary, decimal, even, fromInt, fromString, fromStringAs, fromTLInt, hexadecimal, not, octal, odd, or, pow, shl, shr, toInt, toString, toStringAs, xor)
import Test.Assert (assert)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, resize)
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

-- | Newtype with an Arbitrary instance that generates only small integers
newtype SmallInt = SmallInt Int

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> chooseInt (-5) 5

runSmallInt :: SmallInt -> Int
runSmallInt (SmallInt n) = n

-- | Arbitrary instance for BigInt
newtype TestBigInt = TestBigInt BigInt

derive newtype instance Eq TestBigInt
derive newtype instance Ord TestBigInt
derive newtype instance Semiring TestBigInt
derive newtype instance Ring TestBigInt
derive newtype instance CommutativeRing TestBigInt
derive newtype instance EuclideanRing TestBigInt

instance Arbitrary TestBigInt where
  arbitrary = do
    n <- (fromMaybe zero <<< fromString) <$> digitString
    op <- elements (cons' identity [ negate ])
    pure (TestBigInt (op n))
    where
    digits :: Gen Int
    digits = chooseInt 0 9

    digitString :: Gen String
    digitString = (fold <<< map show) <$> (resize 100 $ arrayOf digits)

-- | Convert SmallInt to BigInt
fromSmallInt :: SmallInt -> BigInt
fromSmallInt = fromInt <<< runSmallInt

-- | Test if a binary relation holds before and after converting to BigInt.
testBinary
  :: (BigInt -> BigInt -> BigInt)
  -> (Int -> Int -> Int)
  -> Effect Unit
testBinary f g = quickCheck (\x y -> (fromInt x) `f` (fromInt y) == fromInt (x `g` y))

main :: Effect Unit
main = do
  log "Simple arithmetic operations and conversions from Int"
  let two = one + one
  let three = two + one
  let four = three + one
  assert $ fromInt 3 == three
  assert $ two * two == four
  assert $ two * three * (three + four) == fromInt 42
  assert $ two - three == fromInt (-1)

  log "Parsing strings"
  assert $ fromString "2" == Just two
  assert $ fromString "a" == Nothing
  assert $ fromString "2.1" == Nothing
  assert $ fromString "123456789" == Just (fromInt 123456789)
  assert $ fromString "10000000" == Just (fromInt 10000000)
  quickCheck $ \(TestBigInt a) -> (fromString <<< toString) a == Just a

  quickCheck $ \(TestBigInt a) -> 
    let radixes = [binary, octal, decimal, hexadecimal, base36]
    in un Conj $ flip foldMap radixes $ \r -> 
          Conj $ (fromStringAs r $ toStringAs r a) == Just a

  log "Parsing strings with a different base"
  assert $ fromString "0b100" == Just four
  assert $ fromString "0xff" == fromString "255"

  log "Conversions between String, Int and BigInt should not loose precision"
  quickCheck (\n -> fromString (show n) == Just (fromInt n))
  assert $ toStringAs binary (fromInt 9) == "1001"
  assert $ toStringAs octal (fromInt 10) == "12"

  log "Binary relations between integers should hold before and after converting to BigInt"
  testBinary (+) (+)
  testBinary (-) (-)
--  testBinary (*) (*)
  testBinary mod mod
  testBinary (/) (/)

  log "Can parse 256 bit numbers"
  assert $ isJust $ fromString "115792089237316195423570985008687907853269984665640564039457584007913129639935"
  assert $ isJust $ fromStringAs hexadecimal "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0cbf"

  -- To test the multiplication, we need to make sure that Int does not overflow
  quickCheck (\x y -> fromSmallInt x * fromSmallInt y == fromInt (runSmallInt x * runSmallInt y))

  log "It should perform multiplications which would lead to imprecise results using Number"
  assert $ Just (fromInt 333190782 * fromInt 1103515245) == fromString "367681107430471590"

  log "compare, (==), even, odd should be the same before and after converting to BigInt"
  quickCheck (\x y -> compare x y == compare (fromInt x) (fromInt y))
  quickCheck (\x y -> (fromSmallInt x == fromSmallInt y) == (runSmallInt x == runSmallInt y))

  log "pow should perform integer exponentiation and yield 0 for negative exponents"
  assert $ three `pow` four == fromInt 81
  assert $ (spy "2" $ three `pow` -two) == (spy "zero" zero)
  assert $ (spy "3" $ three `pow` zero) == one
  assert $ zero `pow` zero == one

  log "Logic"
  assert $ (not <<< not) one == one
  assert $ or one three == three
  assert $ xor one three == two
  assert $ and one three == one

  log "Shifting"
  assert $ shl two one == four
  assert $ shr two one == one

  let prxBigInt = Proxy ∷ Proxy TestBigInt
  Data.checkEq prxBigInt
  Data.checkOrd prxBigInt
  Data.checkSemiring prxBigInt
  Data.checkRing prxBigInt
  Data.checkCommutativeRing prxBigInt
  Data.checkEuclideanRing prxBigInt

  log "Converting BigInt to Int"
  assert $ (fromString "0" >>= toInt) == Just 0
  assert $ (fromString "2137" >>= toInt) == Just 2137
  assert $ (fromString "-2137" >>= toInt) == Just (-2137)
  assert $ (fromString "921231231322337203685124775809" >>= toInt) == Nothing
  assert $ (fromString "-922337203612312312312854775809" >>= toInt) == Nothing
  -- quickCheck (\a b c ->
  --              let x = add (fromInt a) (add (fromInt b) (fromInt c))
  --              in case asIntN 64 x of
  --                Nothing -> x < fromInt (-2147483648) || x > fromInt 2147483647
  --                Just i -> fromInt i == x
  --            )

  log "Type Level Int creation"
  assert $ toString (fromTLInt (Proxy :: Proxy 921231231322337203685124775809)) == "921231231322337203685124775809"
  assert $ toString (fromTLInt (Proxy :: Proxy (-921231231322337203685124775809))) == "-921231231322337203685124775809"

  log "Parity"
  assert $ even (fromInt 42)
  assert $ odd (fromInt 42) == false
  assert $ odd (fromInt 31)
  assert $ even (fromInt 31) == false
