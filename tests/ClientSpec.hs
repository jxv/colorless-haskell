module ClientSpec (spec) where

import qualified Data.Vector as V
import qualified Data.Map as Map
import Test.Hspec
import Data.Aeson
import Data.Aeson.Types
import Data.Int

import Colorless.Types
import Colorless.Ast
import Colorless.Client

helloWorld :: Expr (Int32, Int32)
helloWorld = begin $ do
  x <- def "x" (i32 0)
  f <- defn "f" (fn2 "a" "b" $ \a b -> add a b)
  stmt $ tuple2 x (f -< (x,x))

spec :: Spec
spec = do
  describe "compile" $ do
    it "should compile" $ do
      True `shouldBe` True
