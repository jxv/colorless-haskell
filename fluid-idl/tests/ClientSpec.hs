module ClientSpec (spec) where

import Test.Hspec

import Fluid.Client

_helloWorld :: Expr (Int, Int)
_helloWorld = dO $ do
  x <- def "x" (int 0)
  f <- defn "f" (fn2 "a" "b" addInt)
  stmt $ tuple2 x (f -< (x, int 10))

spec :: Spec
spec = do
  describe "compile" $ do
    it "should compile" $ do
      True `shouldBe` True
