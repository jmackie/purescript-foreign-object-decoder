module Test.Main where

import Prelude

import Data.Either (Either, either)
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Foldable (intercalate)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object.Decode (decodeObject)

main :: Effect Unit
main = do
  either (intercalate "\n" >>> log) logShow (decodeObject good :: Either _ Example)
  either (intercalate "\n" >>> log) logShow (decodeObject bad  :: Either _ Example)

type Example = { x :: Int, y :: String, z :: { zz ::  Boolean } }

foreign import good :: Object Foreign
foreign import bad  :: Object Foreign
