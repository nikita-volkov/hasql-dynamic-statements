module Main where

import Prelude hiding (assert)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Hasql.Statement as Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.DynamicStatements.Session as Session


main =
  defaultMain tree

tree =
  testGroup "All tests"
  [
    testCase "Select substring" $ let
      sample string from to = let
        snippet =
          "select substring(" <> Snippet.param @Text string <>
          foldMap (mappend " from " . Snippet.param @Int32) from <>
          foldMap (mappend " for " . Snippet.param @Int32) to <>
          ")"
        decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text))
        in runSession (Session.dynamicallyParameterizedStatement snippet decoder)
      in do
        assertEqual "" (Right (Right "bc")) =<< sample "abcd" (Just 2) (Just 2)
        assertEqual "" (Right (Right "bcd")) =<< sample "abcd" (Just 2) (Just 3)
        assertEqual "" (Right (Right "abc")) =<< sample "abcd" Nothing (Just 3)
        assertEqual "" (Right (Right "bcd")) =<< sample "abcd" (Just 2) Nothing
  ]

runSession :: Session.Session a -> IO (Either Connection.ConnectionError (Either Session.QueryError a))
runSession = withConnection . Session.run

withConnection :: (Connection.Connection -> IO a) -> IO (Either Connection.ConnectionError a)
withConnection handler =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ Connection.acquire settings
      where
        settings =
          Connection.settings host port user password database
          where
            host = "localhost"
            port = 5432
            user = "postgres"
            password = ""
            database = "postgres"
    use connection =
      lift $ handler connection
    release connection =
      lift $ Connection.release connection
