module Regressions.Issue2Spec where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.DynamicStatements.Snippet qualified as Snippet
import Hasql.Session qualified as Session
import Test.Hspec
import Prelude

spec :: SpecWith Connection.Connection
spec =
  describe "Missing $ for 1000th parameter string error" do
    it "Doesn't happen" \connection -> do
      let snippet =
            "SELECT 1 " <> (foldMap @[] ("," <>) (replicate 1001 (Snippet.param (10 :: Int64))))
      result <- Connection.use connection (Session.statement () (Snippet.toStatement snippet Decoders.noResult))
      case result of
        Right () -> pure ()
        _ -> expectationFailure "Statement execution failed"
