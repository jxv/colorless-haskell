module ClientSpec (spec) where

import Test.Hspec
import Data.Int

import Colorless.Client

_helloWorld :: Expr (Int32, Int32)
_helloWorld = begin $ do
  x <- def "x" (i32 0)
  f <- defn "f" (fn2 "a" "b" add)
  stmt $ tuple2 x (f -< (x,x))

spec :: Spec
spec = do
  describe "compile" $ do
    it "should compile" $ do
      True `shouldBe` True
