module Test.Main where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..), isRight)
import Data.Foldable (intercalate)
import Data.Function.Uncurried as Fn
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign (Foreign)
import Type.Data.Row (RProxy(..))

import Foreign.Object.Decode as Object.Decode

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Singleton int object" do
    it "Decodes" do
      decodeTest exampleA (RProxy :: RProxy A)
        `shouldEqual` Right { x: 2 }

  describe "All the scalar types " do
    it "Decodes" do
      decodeTest exampleB (RProxy :: RProxy B)
        `shouldEqual` Right { a: "string", b: 'c', c: true, d: 200.0, e: 200 }

  describe "Object with a string array" do
    it "Decodes" do
      decodeTest exampleC (RProxy :: RProxy C)
        `shouldEqual` Right { strings: ["foo", "bar"] }

  describe "Object with a nested object" do
    it "Decodes" do
      decodeTest exampleD (RProxy :: RProxy D)
        `shouldEqual` Right { nested: { x: 24.0, y: 42.0 } }

  describe "Object with a callback" do
    it "Decodes" do
      -- void because of the missing Show instance
      void (decodeTest exampleE (RProxy :: RProxy E))
        `shouldSatisfy` isRight

    it "Fails with an arity error" do
      -- void because of the missing Show instance
      void (decodeTest exampleE (RProxy :: RProxy (callback :: Fn.Fn2 Foreign Foreign Foreign)))
        `shouldEqual` Left ".callback: arity mismatch, expecting 2 got 1"

type A = ( x :: Int )
foreign import exampleA :: Foreign -- :: Record A

type B = ( a :: String, b :: Char, c :: Boolean, d :: Number, e :: Int )
foreign import exampleB :: Foreign -- :: Record B

type C = ( strings :: Array String )
foreign import exampleC :: Foreign -- :: Record C

type D = ( nested :: { x :: Number, y :: Number } )
foreign import exampleD :: Foreign -- :: Record D

type E = ( callback :: Foreign -> Foreign )
foreign import exampleE :: Foreign -- :: Record E

decodeTest
  :: forall row
   . Object.Decode.DecodeRow row
  => Foreign
  -> RProxy row
  -> Either String (Record row)
decodeTest value _ =
  lmap (intercalate " & ") (Object.Decode.decodeForeign value)
