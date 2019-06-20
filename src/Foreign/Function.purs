module Foreign.Function
  ( readFn1
  , readFn2
  , readFn3
  , readFn4
  , readFn5
  , readFn6
  , readFn7
  , readFn8
  , readFn9
  , readFn10
  ) where

import Prelude

import Foreign (Foreign)
import Foreign as Foreign
import Data.Function.Uncurried as Fn

readFn1 :: Foreign -> Foreign.F (Fn.Fn1 Foreign Foreign)
readFn1 = unsafeReadFn 1

readFn2 :: Foreign -> Foreign.F (Fn.Fn2 Foreign Foreign Foreign )
readFn2 = unsafeReadFn 2

readFn3 :: Foreign -> Foreign.F (Fn.Fn3 Foreign Foreign Foreign Foreign)
readFn3 = unsafeReadFn 3

readFn4 :: Foreign -> Foreign.F (Fn.Fn4 Foreign Foreign Foreign Foreign Foreign)
readFn4 = unsafeReadFn 4

readFn5 :: Foreign -> Foreign.F (Fn.Fn5 Foreign Foreign Foreign Foreign Foreign Foreign)
readFn5 = unsafeReadFn 5

readFn6 :: Foreign -> Foreign.F (Fn.Fn6 Foreign Foreign Foreign Foreign Foreign Foreign Foreign)
readFn6 = unsafeReadFn 6

readFn7 :: Foreign -> Foreign.F (Fn.Fn7 Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign)
readFn7 = unsafeReadFn 7

readFn8 :: Foreign -> Foreign.F (Fn.Fn8 Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign)
readFn8 = unsafeReadFn 8

readFn9 :: Foreign -> Foreign.F (Fn.Fn9 Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign)
readFn9 = unsafeReadFn 9

readFn10 :: Foreign -> Foreign.F (Fn.Fn10 Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign)
readFn10 = unsafeReadFn 10

unsafeReadFn :: forall a. Int -> Foreign -> Foreign.F a
unsafeReadFn arity value =
  let valueType = Foreign.typeOf value in
  if valueType /= "function" && unsafeArity value == arity
     then pure (Foreign.unsafeFromForeign value)
     else Foreign.fail (Foreign.TypeMismatch ("function " <> show arity <> " argument)") valueType)

foreign import unsafeArity :: Foreign -> Int
