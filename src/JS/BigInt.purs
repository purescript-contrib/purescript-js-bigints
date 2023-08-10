module JS.BigInt
  ( BigInt
  , fromInt
  , fromNumber
  , fromString
  , fromStringAs
  , fromTLInt
  , toInt
  , toNumber
  , asIntN
  , asUintN
  , toString
  , toStringAs
  , pow
  , not
  , and
  , or
  , shl
  , shr
  , xor
  , even
  , odd
  , parity
  , module Exports
  ) where

import Prelude

import Data.Int (Parity(..), Radix)
import Data.Int as Int
import Data.Int (Parity(..), Radix, binary, octal, decimal, hexadecimal) as Exports
import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable, reflectType)
import Prim.Int (class ToString)
import Type.Proxy (Proxy(..))

foreign import data BigInt ∷ Type

-- | FFI wrapper to parse a String into a BigInt.
foreign import fromStringImpl ∷
  ∀ a.
  (a → Maybe a) →
  Maybe a →
  String →
  Maybe BigInt

-- | Parse a string into a `BigInt`. Returns `Nothing` if the parse fails.
-- | Supports decimal, binary, octal, hexadecimal and exponentiation notations.
-- | See [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Numbers_and_dates) for more examples.
-- |
-- | Examples:
-- | ```purescript
-- | fromString "857981209301293808359384092830482"  -- 857981209301293808359384092830482
-- | fromString "0b10000000000000000000000000000000" -- 2147483648
-- | fromString "0B00000000011111111111111111111111" -- 8388607
-- | fromString "0O755"                              -- 493
-- | fromString "0o644"                              -- 420
-- | fromString "0xFFFFFFFFFFFFFFFFF"                -- 295147905179352830000
-- | fromString "0XA"                                -- 10
-- | fromString "0e-5"                               -- 0
-- | fromString "5e1"                                -- 50
-- | fromString "175e-2"                             -- 1.75
-- | fromString "1E3"                                -- 1000
-- | ```
fromString ∷ String → Maybe BigInt
fromString = fromStringImpl Just Nothing

foreign import fromStringAsImpl
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Radix
  -> String
  -> Maybe BigInt

-- | Like `fromString`, but the integer can be specified in a different base.
-- |
-- | Example:
-- | ``` purs
-- | fromStringAs binary      "100" == Just 4
-- | fromStringAs hexadecimal "ff"  == Just 255
-- | ```
fromStringAs :: Radix -> String -> Maybe BigInt
fromStringAs = fromStringAsImpl Just Nothing

-- | Convert an integer to a BigInt.
foreign import fromInt ∷ Int → BigInt

-- | FFI wrapper to parse a Number into a BigInt.
foreign import fromNumberImpl ∷
  ∀ a.
  (a → Maybe a) →
  Maybe a →
  Number →
  Maybe BigInt

-- | Convert a Number to a BigInt. The fractional part is truncated.
fromNumber ∷ Number → Maybe BigInt
fromNumber = fromNumberImpl Just Nothing

-- Note: this function should not be exported!
-- It's only safe if used with type-level integers.
foreign import fromTypeLevelInt ∷ String → BigInt

-- | Converts a type-level integer into a `BigInt`:
-- | ```
-- | import Type.Proxy (Proxy(..))
-- | foo = fromTLInt (Proxy :: Proxy 857981209301293808359384092830482)
-- | ```
fromTLInt ∷ ∀ i sym. ToString i sym ⇒ Reflectable sym String ⇒ Proxy i → BigInt
fromTLInt _ = fromTypeLevelInt (reflectType (Proxy ∷ Proxy sym))

-- | Convert a `BigInt` to a `Number`.
-- | There may be a loss of precision.
-- | If the `BigInt` is [larger than 9_007_199_254_740_991](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER) then the result will be `+Infinity`. If the `BigInt` is [smaller than -9_007_199_254_740_991](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MIN_SAFE_INTEGER) then the result will be `-Infinity`.
foreign import toNumber ∷ BigInt → Number

-- | Convert a `BigInt` to an `Int`.
-- | If the `BigInt` is [larger than 2_147_483_647](https://pursuit.purescript.org/builtins/docs/Prim#t:Int) or [smaller than -2_147_483_648](https://pursuit.purescript.org/builtins/docs/Prim#t:Int) then `Nothing` is returned.
toInt ∷ BigInt → Maybe Int
toInt = toNumber >>> Int.fromNumber

foreign import biAdd ∷ BigInt → BigInt → BigInt
foreign import biMul ∷ BigInt → BigInt → BigInt

foreign import biZero ∷ BigInt

foreign import biOne ∷ BigInt

instance Semiring BigInt where
  add = biAdd
  zero = biZero
  mul = biMul
  one = biOne

foreign import biSub ∷ BigInt → BigInt → BigInt

instance Ring BigInt where
  sub = biSub

foreign import biMod ∷ BigInt → BigInt → BigInt
foreign import biDiv ∷ BigInt → BigInt → BigInt
foreign import biDegree ∷ BigInt → Int

instance CommutativeRing BigInt

instance EuclideanRing BigInt where
  degree = biDegree
  div = biDiv
  mod = biMod

-- | Raise a BigInt to the power of another BigInt.
-- | ```
-- | pow (fromInt 2) (fromInt 3) -- 2^3
-- | ```
foreign import pow ∷ BigInt → BigInt → BigInt

-- | Or the bits.
foreign import or ∷ BigInt → BigInt → BigInt

-- | Invert the bits.
foreign import not ∷ BigInt → BigInt

-- | Exlusive or the bits.
foreign import xor ∷ BigInt → BigInt → BigInt

-- | And the bits.
foreign import and ∷ BigInt → BigInt → BigInt

-- | The BigInt in the first argument shifted to the left by the number of bits specified in the second argument.
-- | Excess bits shifted off to the left are discarded, and zero bits are shifted in from the right.
foreign import shl ∷ BigInt → BigInt → BigInt

-- | The BigInt in the first argument shifted to the right by the number of bits specified in the second argument.
-- | Excess bits shifted off to the right are discarded, and copies of the leftmost bit are shifted in from the left.
-- | This operation is also called "sign-propagating right shift" or "arithmetic right shift", because the sign of the resulting number is the same as the sign of the first operand.
foreign import shr ∷ BigInt → BigInt → BigInt

foreign import biEquals ∷ BigInt → BigInt → Boolean

instance Eq BigInt where
  eq = biEquals

foreign import biCompare ∷ BigInt → BigInt → Int

instance Ord BigInt where
  compare x y = case biCompare x y of
    1 → GT
    0 → EQ
    _ → LT

-- | A decimal representation of the `BigInt` as a `String`.
foreign import toString ∷ BigInt → String

instance Show BigInt where
  show = toString

-- | Clamps a BigInt value to the given number of bits, and returns that value as a signed integer.
foreign import asIntN ∷ Int → BigInt → BigInt

-- | Clamps a BigInt value to the given number of bits, and returns that value as an unsigned integer.
foreign import asUintN ∷ Int → BigInt → BigInt

-- | Returns whether an `BigInt` is `Even` or `Odd`.
-- |
-- | ``` purescript
-- | parity (fromInt 0) == Even
-- | parity (fromInt 1) == Odd
-- | ```
parity ∷ BigInt → Parity
parity n = if even n then Even else Odd

-- | Returns whether an `BigInt` is an even number.
-- |
-- | ``` purescript
-- | even (fromInt 0) == true
-- | even (fromInt 1) == false
-- | ```
even ∷ BigInt → Boolean
even x = x `and` one == zero

-- | The negation of `even`.
-- |
-- | ``` purescript
-- | odd (fromInt 0) == false
-- | odd (fromInt 1) == true
-- | ```
odd ∷ BigInt → Boolean
odd x = x `and` one /= zero

-- | Like `toString`, but the integer can be specified in a different base.
foreign import toStringAs ∷ Radix → BigInt → String
