module Js.BigInt.BigInt
  ( BigInt
  , and
  , asIntN
  , asUintN
  , fromInt
  , fromString
  , fromTLInt
  , not
  , or
  , pow
  , shl
  , shr
  , toString
  , xor
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable, reflectType)
import Prim.Int (class ToString)
import Type.Proxy (Proxy(..))

foreign import data BigInt :: Type

-- | FFI wrapper to parse a String into a BigInt.
foreign import fromStringImpl
  :: forall a
   . (a -> Maybe a)
  -> Maybe a
  -> String
  -> Maybe BigInt

-- | Parse a string into a `BigInt`, assuming a decimal representation. Returns
-- | `Nothing` if the parse fails.
-- |
-- | Examples:
-- | ```purescript
-- | fromString "42"
-- | fromString "857981209301293808359384092830482"
-- | fromString "1e100"
-- | ```
fromString ∷ String → Maybe BigInt
fromString = fromStringImpl Just Nothing

-- | Convert an integer to a BigInt.
foreign import fromInt :: Int -> BigInt

-- Note: this function should not be exported!
-- It's only safe if used with type-level integers.
foreign import fromTypeLevelInt :: String -> BigInt

-- | Converts a type-level integer into a `BigInt`:
-- | ```
-- | import Type.Proxy (Proxy(..))
-- | foo = fromTLInt (Proxy :: Proxy 857981209301293808359384092830482)
-- | ```
fromTLInt :: forall i sym. ToString i sym => Reflectable sym String => Proxy i -> BigInt
fromTLInt _ = fromTypeLevelInt (reflectType (Proxy :: Proxy sym))

foreign import biAdd :: BigInt -> BigInt -> BigInt
foreign import biMul :: BigInt -> BigInt -> BigInt

foreign import biZero :: BigInt

foreign import biOne :: BigInt

instance Semiring BigInt where
  add = biAdd
  zero = biZero
  mul = biMul
  one = biOne

foreign import biSub :: BigInt -> BigInt -> BigInt

instance Ring BigInt where
  sub = biSub

foreign import biMod :: BigInt -> BigInt -> BigInt

instance CommutativeRing BigInt

-- Raise an BigInt to the power of another BigInt.
foreign import pow :: BigInt -> BigInt -> BigInt

-- | or the bits.
foreign import or :: BigInt -> BigInt -> BigInt

-- | Invert the bits.
foreign import not :: BigInt -> BigInt

-- | Exlusive or the bits.
foreign import xor :: BigInt -> BigInt -> BigInt

-- | and the bits.
foreign import and :: BigInt -> BigInt -> BigInt

-- | shift the bits left and zero fill.
foreign import shl :: BigInt -> BigInt -> BigInt

-- | Shift the bits right and maintain pos/neg.
foreign import shr :: BigInt -> BigInt -> BigInt

foreign import biEquals :: BigInt -> BigInt -> Boolean

instance Eq BigInt where
  eq = biEquals

foreign import biCompare :: BigInt -> BigInt -> Int

instance Ord BigInt where
  compare x y = case biCompare x y of
    1 -> GT
    0 -> EQ
    _ -> LT

-- | A decimal representation of the `BigInt` as a `String`.
foreign import toString :: BigInt -> String

-- | Clamps a BigInt value to the given number of bits, and returns that value as a signed integer.
foreign import asIntN :: Int -> BigInt -> BigInt

-- | Clamps a BigInt value to the given number of bits, and returns that value as an unsigned integer.
foreign import asUintN :: Int -> BigInt -> BigInt
