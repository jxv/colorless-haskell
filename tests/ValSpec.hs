module ValSpec (spec) where

import qualified Data.Map as Map
import Test.Hspec
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)

import Colorless.Server.Types
import Colorless.Server.Val

spec :: Spec
spec = do
  describe "parse basic types from json" $ do
    it "Enumeral" $ shouldBe
      (parseMaybe parseJSON $ object [ "tag" .= ("None" :: Text) ])
      (Just $ Val'ApiVal $ ApiVal'Enumeral $ Enumeral "None" Nothing)
    it "Struct" $ shouldBe
      (parseMaybe parseJSON $ object [ "x" .= True ])
      (Just $ Val'ApiVal $ ApiVal'Struct $ Struct $ Map.fromList [("x", Val'Const $ Const'Bool True)] )
    it "Struct" $ shouldBe
      (parseMaybe parseJSON $ object [ "x" .= object [ "tag" .= ("None" :: Text) ] ])
      (Just $ Val'ApiVal $ ApiVal'Struct $ Struct $ Map.fromList [("x", Val'ApiVal $ ApiVal'Enumeral $ Enumeral "None" Nothing)] )
  describe "parse types from val" $ do
    context "Option" $ do
      it "None" $ shouldBe
        (fromVal $ Val'Const Const'Null)
        (Just Nothing :: Maybe (Maybe Bool))
      it "Some" $ shouldBe
        (fromVal $ Val'Const $ Const'Bool $ True)
        (Just (Just True) :: Maybe (Maybe Bool))
