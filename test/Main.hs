module Main where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Session as Session
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.DynamicStatements.Statement as Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude hiding (assert)

main =
  defaultMain tree

tree =
  testGroup
    "All tests"
    [ testCase "Select substring"
        $ let sample string from to =
                let snippet =
                      "select substring("
                        <> Snippet.param @Text string
                        <> foldMap (mappend " from " . Snippet.param @Int32) from
                        <> foldMap (mappend " for " . Snippet.param @Int32) to
                        <> ")"
                    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text))
                 in runSession (Session.dynamicallyParameterizedStatement snippet decoder True)
           in do
                assertEqual "" (Right (Right "bc")) =<< sample "abcd" (Just 2) (Just 2)
                assertEqual "" (Right (Right "bcd")) =<< sample "abcd" (Just 2) (Just 3)
                assertEqual "" (Right (Right "abc")) =<< sample "abcd" Nothing (Just 3)
                assertEqual "" (Right (Right "bcd")) =<< sample "abcd" (Just 2) Nothing,
      testGroup
        "Regression"
        [ testCase "Missing $ for 1000th parameter string #2"
            $ let snippet =
                    "SELECT 1 " <> (foldMap @[] ("," <>) $ replicate 1001 $ Snippet.param (10 :: Int64))
                  statement =
                    Statement.dynamicallyParameterized snippet Decoders.noResult True
                  sql =
                    case statement of
                      Statement.Statement x _ _ _ -> x
               in do
                    assertBool (ByteStringChar8.unpack sql) (ByteString.isInfixOf "$1000" sql)
        ]
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
            password = "postgres"
            database = "postgres"
    use connection =
      lift $ handler connection
    release connection =
      lift $ Connection.release connection
