module Units.SnippetSpec where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.DynamicStatements.Session qualified as Session
import Hasql.DynamicStatements.Snippet qualified as Snippet
import Test.Hspec
import Prelude

spec :: SpecWith Connection.Connection
spec = do
  describe "Select substring" do
    it "Works" \connection -> do
      let sample string from to =
            let snippet =
                  "select substring("
                    <> Snippet.param @Text string
                    <> foldMap (mappend " from " . Snippet.param @Int32) from
                    <> foldMap (mappend " for " . Snippet.param @Int32) to
                    <> ")"
                decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text))
             in Connection.use connection (Session.dynamicallyParameterizedStatement snippet decoder)
       in do
            shouldBe (Right "bc") =<< sample "abcd" (Just 2) (Just 2)
            shouldBe (Right "bcd") =<< sample "abcd" (Just 2) (Just 3)
            shouldBe (Right "abc") =<< sample "abcd" Nothing (Just 3)
            shouldBe (Right "bcd") =<< sample "abcd" (Just 2) Nothing
