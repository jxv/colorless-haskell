module ValSpec (spec) where

import qualified Data.Map as Map
import Test.Hspec
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)

import Colorless.Runtime.Types
import Colorless.Runtime.Val

spec :: Spec
spec = do
  describe "parse basic types from json" $ do
    it "Enumerator" $ shouldBe
      (parseMaybe parseJSON $ object [ "tag" .= ("None" :: Text) ])
      (Just $ Val'ApiVal $ ApiVal'Enumerator $ Enumerator "None" Nothing)
    it "Struct" $ shouldBe
      (parseMaybe parseJSON $ object [ "x" .= True ])
      (Just $ Val'ApiVal $ ApiVal'Struct $ Struct $ Map.fromList [("x", Val'Const $ Const'Bool True)] )
    it "Struct" $ shouldBe
      (parseMaybe parseJSON $ object [ "x" .= object [ "tag" .= ("None" :: Text) ] ])
      (Just $ Val'ApiVal $ ApiVal'Struct $ Struct $ Map.fromList [("x", Val'ApiVal $ ApiVal'Enumerator $ Enumerator "None" Nothing)] )
  describe "parse types from val" $ do
    context "Option" $ do
      it "None" $ shouldBe
        (fromVal $ Val'ApiVal $ ApiVal'Enumerator $ Enumerator "None" Nothing)
        (Just Nothing :: Maybe (Maybe Bool))
      it "Some" $ shouldBe
        (fromVal $ Val'ApiVal $ ApiVal'Enumerator $ Enumerator "Some" $ Just $ Map.fromList [("some", Val'Const $ Const'Bool $ True)])
        (Just (Just True) :: Maybe (Maybe Bool))
