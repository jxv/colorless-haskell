module AstSpec (spec) where

import qualified Data.Vector as V
import qualified Data.Map as Map
import Test.Hspec
import Data.Aeson
import Data.Aeson.Types

import Colorless.Server.Types
import Colorless.Server.Ast

parseAst :: Value -> Maybe Ast
parseAst = parseMaybe parseJSON

spec :: Spec
spec = do
  describe "parse basic types" $ do
    it "TypeName" $ shouldBe
      (parseMaybe parseJSON (String "MyName"))
      (Just $ TypeName "MyName")

    it "MemberName" $ shouldBe
      (parseMaybe parseJSON (String "MyName"))
      (Just $ MemberName "MyName")

    context "Type" $ do
      it "string only" $ shouldBe
        (parseMaybe parseJSON (String "MyType"))
        (Just $ Type "MyType" Nothing)

      it "object with no param" $ shouldBe
        (parseMaybe parseJSON $ object ["n" .= String "MyType"])
        (Just $ Type "MyType" Nothing)

      it "object with param" $ shouldBe
        (parseMaybe parseJSON $ object ["n" .= String "MyType", "p" .= String "MyParam"])
        (Just $ Type "MyType" (Just $ Type "MyParam" Nothing))

  describe "parse Ast" $ do
    it "Ref" $ shouldBe
      (parseAst $ object ["@" .= String "myRef"])
      (Just $ Ast'Ref $ Ref "myRef")

    context "If" $ do
      it "simple" $ shouldBe
        (parseAst $ Array $ V.fromList ["if", Bool True, "true", "false"])
        (Just $ Ast'If $ If
          (Ast'Const $ Const'Bool True)
          (Ast'Const $ Const'String "true")
          (Ast'Const $ Const'String "false"))

    context "Get" $ do
      it "simple yet not semantically correct" $ shouldBe
        (parseAst $ Array $ V.fromList ["get", Array $ V.fromList ["a","b","c"], Bool False])
        (Just $ Ast'Get $ Get ["a","b","c"] (Ast'Const $ Const'Bool False))

    context "Define" $ do
      it "simple" $ shouldBe
        (parseAst $ Array $ V.fromList ["def", "myVar", Bool True])
        (Just $ Ast'Define $ Define "myVar" (Ast'Const $ Const'Bool True))

    context "Lambda" $ do
      it "simple" $ shouldBe
        (parseAst $ Array $ V.fromList ["fn", Array $ V.fromList [ object [ "myVar" .= String "Int64" ] ], Bool True])
        (Just $ Ast'Lambda $ Lambda [("myVar", Type "Int64" Nothing)] (Ast'Const $ Const'Bool True))

    context "List" $ do
      it "simple" $ shouldBe
        (parseAst $ object [ "List" .= [String "hello"] ])
        (Just $ Ast'List $ List [Ast'Const $ Const'String "hello"])

    context "Tuple" $ do
      it "one element (sematically too small)" $ shouldBe
        (parseAst $ object [ "Tuple" .= [String "hello"] ])
        (Just $ Ast'Tuple $ Tuple [Ast'Const $ Const'String "hello"])

      it "more than one elements" $ shouldBe
        (parseAst $ object [ "Tuple" .= [String "hello", String "world"] ])
        (Just $ Ast'Tuple $ Tuple [Ast'Const $ Const'String "hello", Ast'Const $ Const'String "world"])

    context "Begin" $ do
      it "simple" $ shouldBe
        (parseAst $ Array $ V.fromList ["begin", Bool True])
        (Just $ Ast'Begin $ Begin [Ast'Const $ Const'Bool True])

    context "FnCall" $ do
      it "simple" $ shouldBe
        (parseAst $ Array $ V.fromList ["someFn", "one", Bool False])
        (Just $ Ast'FnCall $ FnCall (Ast'Ref $ Ref "someFn") [Ast'Const $ Const'String "one", Ast'Const $ Const'Bool False])

    context "EnumerationCall" $ do
      it "simple" $ shouldBe
        (parseAst $ object [ "n" .= String "MyEnum", "e" .= object [ "tag" .= String "MyTag" ] ])
        (Just $ Ast'EnumerationCall $ EnumerationCall "MyEnum" (Ast'Enumeral $ Enumeral "MyTag" Nothing))

    context "StructCall" $ do
      it "simple" $ shouldBe
        (parseAst $ object [ "n" .= String "MyStruct", "m" .= object [ "x" .= Bool True ] ])
        (Just $ Ast'StructCall $ StructCall "MyStruct" (Map.fromList [("x", Ast'Const $ Const'Bool True)]))

    context "HollowCall" $ do
      it "simple" $ shouldBe
        (parseAst $ object [ "n" .= String "MyCall" ])
        (Just $ Ast'HollowCall $ HollowCall "MyCall")

    context "Enumeral" $ do
      it "simple without members" $ shouldBe
        (parseAst $ object [ "tag" .= String "MyTag" ])
        (Just $ Ast'Enumeral $ Enumeral "MyTag" Nothing)
      it "simple with members" $ shouldBe
        (parseAst $ object [ "tag" .= String "MyTag", "x" .= Bool True ])
        (Just $ Ast'Enumeral $ Enumeral "MyTag" (Just $ Map.fromList [("x", Ast'Const $ Const'Bool True)]))

    context "Struct" $ do
      it "simple" $ shouldBe
        (parseAst $ object [ "key0" .= Bool True ])
        (Just $ Ast'Struct $ Struct $ Map.fromList [("key0", Ast'Const $ Const'Bool True)])

    context "Const" $ do
      it "Bool" $ shouldBe
        (parseAst $ Bool True)
        (Just $ Ast'Const $ Const'Bool True)
      it "String" $ shouldBe
        (parseAst $ String "Hello")
        (Just $ Ast'Const $ Const'String "Hello")
      it "Number" $ shouldBe
        (parseAst $ Number 12345)
        (Just $ Ast'Const $ Const'Number 12345)
