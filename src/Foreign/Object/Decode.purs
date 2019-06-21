module Foreign.Object.Decode
  ( decodeForeign
  , decodeObject
  , class DecodeValue, decodeValue

  , class DecodeRow, decodeRow
  , class DecodeRowList, decodeRowList
  ) where

import Prelude

import Control.Monad.Except (runExcept, withExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Function.Uncurried as Fn
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Function as Foreign.Function
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.Row (RProxy(..))
import Type.RowList (class ListToRow, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

-- | Attempt to decode a javascript value into a record type, provided all the fields
-- | of the record can be decoded from javascript values (i.e. the `DecodeRow`
-- | constraint).
decodeForeign
  :: forall row
   . DecodeRow row
  => Foreign
  -> Either (NonEmptyList String) (Record row)
decodeForeign value =
  let valueType = Foreign.typeOf value in
  if valueType /= "object"
     then let error = Foreign.TypeMismatch "object" valueType in
          Left (NonEmptyList.singleton (renderError error))
     else decodeObject (Foreign.unsafeFromForeign value)

-- | Attempt to decode a javascript object into a record type, provided all the fields
-- | of the record can be decoded from javascript values (i.e. the `DecodeRow`
-- | constraint).
decodeObject
  :: forall row
   . DecodeRow row
  => Object Foreign
  -> Either (NonEmptyList String) (Record row)
decodeObject object =
  case runExcept (decodeRow (RProxy :: RProxy row) object) of
    Left errors -> Left (renderErrors errors)
    Right builder -> Right (Builder.build builder {})

renderErrors :: Foreign.MultipleErrors -> NonEmptyList String
renderErrors = map renderError

renderError :: Foreign.ForeignError -> String
renderError = gatherContext [] >>> case _ of
  Tuple [] error -> error
  Tuple context error -> intercalate "" context <> ": " <> error
  where
  gatherContext context = case _ of
    Foreign.ForeignError error ->
      Tuple context error

    Foreign.TypeMismatch expected got ->
      Tuple context ("type mismatch, expected " <> expected <> " but got " <> got)

    Foreign.ErrorAtIndex i error ->
      gatherContext (Array.snoc context ("[" <> show i <> "]")) error

    Foreign.ErrorAtProperty key error ->
      gatherContext (Array.snoc context ("." <> key)) error

-- | Class that powers the `decodeForeign` and `decodeObject` functions.
-- | You can ignore it.
class DecodeRow (row :: # Type) where
  decodeRow :: RProxy row -> Object Foreign -> Foreign.F (Builder {} (Record row))

instance decodeRowImpl :: (RowToList row list, DecodeRowList list row) => DecodeRow row where
  decodeRow _ = decodeRowList (RLProxy :: RLProxy list)

-- | Class that powers the `DecodeRow` class, which powers the`decodeForeign`
-- | and `decodeObject` functions. You can ignore it.
class ListToRow list row <= DecodeRowList (list :: RowList) row where
  decodeRowList :: RLProxy list -> Object Foreign -> Foreign.F (Builder {} (Record row))

instance decodeRowListConsNil
  :: ( Cons label a () row
     , Lacks label ()
     , IsSymbol label
     , DecodeValue a
     )
  => DecodeRowList (Cons label a Nil) row where

  decodeRowList _ object =
    case Object.pop (reflectSymbol labelProxy) object of
      Nothing ->
        Foreign.fail $
          Foreign.ErrorAtProperty label $
            Foreign.ForeignError "missing value"

      Just (Tuple value remaining) ->
        case Object.keys remaining of
          [] -> Builder.insert labelProxy <$>
                withExcept (map (Foreign.ErrorAtProperty label)) (decodeValue value)
          keys ->
            Foreign.fail $
              Foreign.ErrorAtProperty label $
                Foreign.ForeignError ("unexpected keys: " <> intercalate ", " keys)
    where
    label :: String
    label = reflectSymbol labelProxy

    labelProxy :: SProxy label
    labelProxy = SProxy

else
instance decodeRowListCons
  :: ( DecodeRowList rest tail
     , Cons label a tail row
     , Lacks label tail
     , IsSymbol label
     , DecodeValue a
     )
  => DecodeRowList (Cons label a rest) row where

  decodeRowList _ object =
    case Object.pop label object of
      Nothing ->
        Foreign.fail $
          Foreign.ErrorAtProperty label $
            Foreign.ForeignError "missing value"

      Just (Tuple value remaining) -> do
        x <- Builder.insert labelProxy <$>
             withExcept (map (Foreign.ErrorAtProperty label)) (decodeValue value)
        y <- decodeRowList (RLProxy :: RLProxy rest) remaining
        pure (y >>> x)
    where
    label :: String
    label = reflectSymbol labelProxy

    labelProxy :: SProxy label
    labelProxy = SProxy

-- | Classy `read*` functions.
-- |
-- | In order to be an instance of `DecodeRow`, all fields of a record must have
-- | instances of `DecodeValue`.
class DecodeValue a where
  decodeValue :: Foreign -> Foreign.F a

instance decodeValueString  :: DecodeValue String  where decodeValue = Foreign.readString
instance decodeValueChar    :: DecodeValue Char    where decodeValue = Foreign.readChar
instance decodeValueBoolean :: DecodeValue Boolean where decodeValue = Foreign.readBoolean
instance decodeValueNumber  :: DecodeValue Number  where decodeValue = Foreign.readNumber
instance decodeValueInt     :: DecodeValue Int     where decodeValue = Foreign.readInt

instance decodeValueArray :: DecodeValue a => DecodeValue (Array a) where
  decodeValue = Foreign.readArray >=> traverse decodeValue

instance decodeValueMaybe :: DecodeValue a => DecodeValue (Maybe a) where
  decodeValue = Foreign.readNullOrUndefined >=> maybe (pure Nothing) decodeValue

instance decodeValueRecord :: DecodeRow row => DecodeValue (Record row) where
  decodeValue value =
    let valueType = Foreign.typeOf value in
    if valueType == "object"
       then flip Builder.build {} <$> decodeRow (RProxy :: RProxy row) (Foreign.unsafeFromForeign value)
       else Foreign.fail (Foreign.TypeMismatch "object" valueType)

instance decodeValueFn1  :: DecodeValue (Foreign -> Foreign) where decodeValue = Foreign.Function.readFn1
instance decodeValueFn2  :: DecodeValue (Fn.Fn2 Foreign Foreign Foreign) where decodeValue = Foreign.Function.readFn2
instance decodeValueFn3  :: DecodeValue (Fn.Fn3 Foreign Foreign Foreign Foreign) where decodeValue = Foreign.Function.readFn3
instance decodeValueFn4  :: DecodeValue (Fn.Fn4 Foreign Foreign Foreign Foreign Foreign) where decodeValue = Foreign.Function.readFn4
instance decodeValueFn5  :: DecodeValue (Fn.Fn5 Foreign Foreign Foreign Foreign Foreign Foreign) where decodeValue = Foreign.Function.readFn5
instance decodeValueFn6  :: DecodeValue (Fn.Fn6 Foreign Foreign Foreign Foreign Foreign Foreign Foreign) where decodeValue = Foreign.Function.readFn6
instance decodeValueFn7  :: DecodeValue (Fn.Fn7 Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign) where decodeValue = Foreign.Function.readFn7
instance decodeValueFn8  :: DecodeValue (Fn.Fn8 Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign) where decodeValue = Foreign.Function.readFn8
instance decodeValueFn9  :: DecodeValue (Fn.Fn9 Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign) where decodeValue = Foreign.Function.readFn9
instance decodeValueFn10 :: DecodeValue (Fn.Fn10 Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign Foreign) where decodeValue = Foreign.Function.readFn10
