module Hasql.DynamicStatements.Statement where

import Hasql.DynamicStatements.Prelude
import Hasql.Statement
import qualified Hasql.DynamicStatements.Snippet.Defs as SnippetDefs
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Ptr.Poking as Poking
import qualified Ptr.ByteString as ByteString

{-|
Construct a statement dynamically, specifying the parameters in-place.
-}
snippet :: SnippetDefs.Snippet -> Decoders.Result result -> Statement () result
snippet (SnippetDefs.Snippet _ chunks) decoder = let
  step (!paramId, !poking, !encoder) = \ case
    SnippetDefs.TextSnippetChunk sql -> (paramId, poking <> Poking.bytes sql, encoder)
    SnippetDefs.ParamSnippetChunk paramEncoder -> let
      newParamId = paramId + 1
      newPoking = poking <> Poking.word8 36 <> Poking.asciiIntegral paramId
      newEncoder = encoder <> Encoders.param paramEncoder
      in (newParamId, newPoking, newEncoder)
  in case foldl' step (1, mempty, mempty) chunks of
    (_, poking, encoder) -> Statement (ByteString.poking poking) encoder decoder False
