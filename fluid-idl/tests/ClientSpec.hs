module ClientSpec (spec) where

import Test.Hspec
import Data.Int

import Fluid.Client

_helloWorld :: Expr (Int32, Int32)
_helloWorld = dO $ do
  x <- def "x" (i32 0)
  f <- defn "f" (fn2 "a" "b" add)
  stmt $ tuple2 x (f -< (x, i32 10))

spec :: Spec
spec = do
  describe "compile" $ do
    it "should compile" $ do
      True `shouldBe` True
