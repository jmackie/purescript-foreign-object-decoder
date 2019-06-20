module Test.Main where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign (Foreign)
import Type.Data.Row (RProxy(..))

import Foreign.Object.Decode as Object.Decode

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Foreign.Object.Decode" do

    describe "Singleton Int object" do
      it "decodes" do
        decodeTest exampleA (RProxy :: RProxy A)
          `shouldEqual` Right { x: 2 }

    describe "All the scalar types " do
      it "decodes" do
        decodeTest exampleB (RProxy :: RProxy B)
          `shouldEqual` Right { a: "string", b: 'c', c: true, d: 200.0, e: 200 }

type A = ( x :: Int )
foreign import exampleA :: Foreign -- :: Record A

type B = ( a :: String, b :: Char, c :: Boolean, d :: Number, e :: Int )
foreign import exampleB :: Foreign -- :: Record B

decodeTest
  :: forall row
   . Object.Decode.DecodeRow row
  => Foreign
  -> RProxy row
  -> Either String (Record row)
decodeTest value _ =
  lmap (intercalate " & ") (Object.Decode.decodeForeign value)
