module Hasql.DynamicStatements.Session where

import Hasql.DynamicStatements.Prelude
import Hasql.Session
import qualified Hasql.DynamicStatements.Snippet.Defs as SnippetDefs
import qualified Hasql.DynamicStatements.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Session as Session

{-|
Execute a dynamically constructed statement, providing a result decoder.
-}
snippet :: SnippetDefs.Snippet -> Decoders.Result result -> Session result
snippet snippet decoder = Session.statement () (Statement.snippetAndDecoder snippet decoder)
