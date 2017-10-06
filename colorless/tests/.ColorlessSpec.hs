module ColorlessSpec (spec, User(username), GetUserIdByUsername(username)) where

import qualified Data.Map as Map
import Control.Monad (mzero)
import Test.Hspec
import Data.Aeson
import GHC.Generics
import Data.Aeson.Types hiding (Options)
import Data.Text

import Colorless.Runtime
import Colorless.Types

data User = User
  { userId :: Text
  , username :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON User
instance FromJSON User

data GetUser = GetUser
  { userId :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON GetUser
instance FromJSON GetUser

data GetUserIdByUsername = GetUserIdByUsername
  { username :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON GetUserIdByUsername
instance FromJSON GetUserIdByUsername

data CreateUser = CreateUser
  { user :: User
  } deriving (Show, Eq, Generic)

instance ToJSON CreateUser
instance FromJSON CreateUser

data Api
  = Api'GetUser GetUser
  | Api'GetUserIdByUsername GetUserIdByUsername
  | Api'CreateUser CreateUser
  deriving (Show, Eq)

instance ToJSON Api where
  toJSON = \case
    Api'GetUser a -> object [ "n" .= ("GetUser" :: String), "m" .= a ]
    Api'GetUserIdByUsername a -> object [ "n" .= ("GetUserIdByUsername" :: String), "m" .= a ]
    Api'CreateUser a -> object [ "n" .= ("CreateUser" :: String), "m" .= a ]

instance FromJSON Api where
  parseJSON (Object o) = do
    name <- o .: "n"
    case name :: Text of
      "GetUser" -> Api'GetUser <$> o .: "m"
      "GetUserIdByUsername" -> Api'GetUserIdByUsername <$> o .: "m"
      "CreateUser" -> Api'CreateUser <$> o .: "m"
      _ -> mzero
  parseJSON _ = mzero

evalApi :: Value -> IO Value
evalApi = \v -> case parseMaybe parseJSON v :: Maybe Api of
  Nothing -> error "can't parse into api"
  Just api -> case api of
    Api'GetUser GetUser{userId} -> do
      return . toJSON $ User userId "my-username"
    Api'GetUserIdByUsername GetUserIdByUsername{} -> do
      return . toJSON $ String "my-user-id"
    Api'CreateUser CreateUser{user = User{userId}} -> do
      return . toJSON $ userId

spec :: Spec
spec = do
  describe "colorless eval" $ do
    it "test0" $ do
      envRef <- emptyEnv
      let val = Val'Begin $ Begin
            [ Val'Value $ Bool True
            ]
      let cfg = EvalConfig (Options Nothing) evalApi
      val' <- runEval (eval val envRef) cfg
      val' `shouldBe` (Val'Value $ Bool True)

    it "test1" $ do
      envRef <- emptyEnv
      let val = Val'Begin $ Begin
            [ Val'Define $ Define "x" (Val'Value $ Bool False)
            , Val'Atom $ Atom "x"
            ]
      let cfg = EvalConfig (Options Nothing) evalApi
      val' <- runEval (eval val envRef) cfg
      val' `shouldBe` (Val'Value $ Bool False)

    it "test2" $ do
      envRef <- emptyEnv
      let val = Val'Begin $ Begin
            [ Val'Call $ Call
                (Val'Lambda $ Lambda [Map.singleton "x" (Val'Atom $ Atom "I64")] (Val'Atom $ Atom "x"))
                [Val'Value $ Bool True]
            ]
      let cfg = EvalConfig (Options Nothing) evalApi
      val' <- runEval (eval val envRef) cfg
      val' `shouldBe` (Val'Value $ Bool True)

    it "test3" $ do
      envRef <- emptyEnv
      let val = Val'Begin $ Begin
            [ Val'Define $ Define
                "myFunc"
                (Val'Lambda $ Lambda [Map.singleton "x" (Val'Atom $ Atom "I64")] (Val'Atom $ Atom "x"))
            , Val'Call $ Call
                (Val'Atom $ Atom "myFunc")
                [Val'Value $ Bool False]
            ]
      let cfg = EvalConfig (Options Nothing) evalApi
      val' <- runEval (eval val envRef) cfg
      val' `shouldBe` (Val'Value $ Bool False)

    it "test4" $ do
      envRef <- emptyEnv
      let val = Val'Begin $ Begin
            [ Val'Define $ Define
                "x"
                (Val'Value (String "asdf"))
            , Val'Atom $ Atom "x"
            ]
      let cfg = EvalConfig (Options Nothing) evalApi
      val' <- runEval (eval val envRef) cfg
      val' `shouldBe` (Val'Value $ String "asdf")

    it "test5" $ do
      envRef <- emptyEnv
      let j = toJSON [ "begin",
              toJSON
                [ "def"
                , "userId"
                , object [ "n" .= ("GetUserIdByUsername" :: Text), "m" .= object [ "username" .= ("asdf" :: Text) ] ]
                ],
              object
                [ "n" .= ("GetUser" :: Text)
                , "m" .= object [ "userId" .= object [ "@" .= ("userId" :: Text)] ]
                ]
            ]
      let Just val = parseMaybe parseJSON j
      let cfg = EvalConfig (Options Nothing) evalApi
      val' <- runEval (eval val envRef) cfg
      val' `shouldBe` (Val'Value $ toJSON $ User "my-user-id" "my-username")

    it "test6" $ do
      envRef <- emptyEnv
      let j = toJSON [ "begin",
              toJSON
                [ "def"
                , "userId"
                , String "hello"
                ],
              object
                [ "n" .= ("GetUser" :: Text)
                , "m" .= object [ "userId" .= object [ "@" .= ("userId" :: Text)] ]
                ]
            ]
      let Just val = parseMaybe parseJSON j
      let cfg = EvalConfig (Options Nothing) evalApi
      val' <- runEval (eval val envRef) cfg
      val' `shouldBe` (Val'Value $ toJSON $ User "hello" "my-username")

    it "test7" $ do
      envRef <- emptyEnv
      let j = toJSON [ "begin",
                toJSON
                [ "def"
                , "user"
                , object
                  [ "n" .= ("GetUser" :: Text)
                  , "m" .= object [ "userId" .= ("012345" :: Text) ]
                  ]
                ],
              object
                [ "n" .= ("CreateUser" :: Text)
                , "m" .= object [ "user" .= object [ "@" .= ("user" :: Text)] ]
                ]
            ]
      let Just val = parseMaybe parseJSON j
      let cfg = EvalConfig (Options Nothing) evalApi
      val' <- runEval (eval val envRef) cfg
      val' `shouldBe` (Val'Value $ String "012345")

    it "test8" $ do
      envRef <- emptyEnv
      let atom :: Text -> Value
          atom symbol = object [ "@" .= symbol ]
      let j = toJSON [ "begin",
                toJSON
                [ "def"
                , "math"
                , toJSON
                  [ "fn"
                  , toJSON
                    [ object [ "x" .= ("I64" :: Text) ]
                    , object [ "y" .= ("I64" :: Text) ]
                    ]
                  , toJSON
                    [ "+", toJSON [ "*", atom "x", atom "y" ], atom "x" ]
                  ]
                ],
                toJSON [ "math", Number 8, Number 2 ]
            ]
      let Just val = parseMaybe parseJSON j
      let cfg = EvalConfig (Options Nothing) evalApi
      val' <- runEval (eval val envRef) cfg
      val' `shouldBe` (Val'Value $ Number 24)


    it "test9" $ do
      envRef <- emptyEnv
      let atom :: Text -> Value
          atom symbol = object [ "@" .= symbol ]
      let j = toJSON [ "begin",
                toJSON
                [ "def", "user", object [ "n" .= ("GetUser" :: Text), "m" .= object [ "userId" .= ("01234" :: Text) ] ] ],
                toJSON
                [ "get", toJSON [ "username" :: Text ], atom "user" ]
            ]
      let Just val = parseMaybe parseJSON j
      let cfg = EvalConfig (Options Nothing) evalApi
      val' <- runEval (eval val envRef) cfg
      val' `shouldBe` (Val'Value $ String "my-username")
