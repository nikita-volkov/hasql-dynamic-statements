module Hasql.DynamicStatements.Snippet.Defs where

import Hasql.DynamicStatements.Prelude
import Hasql.Encoders qualified as Encoders
import Hasql.Implicits.Encoders qualified as Encoders
import TextBuilder qualified

-- |
-- Composable SQL snippet with parameters injected.
-- Abstracts over placeholders and matching of encoders.
--
-- It has an instance of `IsString`, so having the @OverloadedStrings@ extension enabled
-- you can construct it directly from string literals.
--
-- Here's an example:
--
-- @
-- selectSubstring :: Text -> Maybe Int32 -> Maybe Int32 -> 'Snippet'
-- selectSubstring string from to =
--   "select substring(" <> 'param' string <>
--   'foldMap' (\\ x -> " from " <> 'param' x) from <>
--   'foldMap' (\\ x -> " for " <> 'param' x) to <>
--   ")"
-- @
--
-- Having a decoder you can lift it into 'Hasql.Statement.Statement' using
-- 'Hasql.DynamicStatements.Statement.dynamicallyParameterized' or directly execute it in
-- 'Hasql.Session.Session' using
-- 'Hasql.DynamicStatements.Session.dynamicallyParameterizedStatement'.
newtype Snippet = Snippet (Seq SnippetChunk)

data SnippetChunk
  = StringSnippetChunk TextBuilder.TextBuilder
  | ParamSnippetChunk (Encoders.Params ())

deriving instance Semigroup Snippet

deriving instance Monoid Snippet

instance IsString Snippet where
  fromString x = Snippet (pure (StringSnippetChunk (fromString x)))

-- |
-- SQL chunk.
sql :: Text -> Snippet
sql x = Snippet (pure (StringSnippetChunk (TextBuilder.text x)))

-- |
-- Parameter encoded using an implicitly derived encoder from the type.
param :: (Encoders.DefaultParamEncoder param) => param -> Snippet
param = encoderAndParam Encoders.defaultParam

-- |
-- Parameter with an explicitly defined encoder.
encoderAndParam :: Encoders.NullableOrNot Encoders.Value param -> param -> Snippet
encoderAndParam encoder param = Snippet (pure (ParamSnippetChunk (param >$ Encoders.param encoder)))

compile :: Snippet -> (Text, Encoders.Params ())
compile (Snippet chunks) =
  let step (!paramId, !builder, !encoder) = \case
        StringSnippetChunk sql ->
          (paramId, builder <> sql, encoder)
        ParamSnippetChunk paramEncoder ->
          let newParamId = paramId + 1
              newPoking = builder <> "$" <> TextBuilder.decimal paramId
              newEncoder = encoder <> paramEncoder
           in (newParamId, newPoking, newEncoder)
      (_, builder, encoder) = foldl' step (1 :: Int, mempty, mempty) chunks
      sql = TextBuilder.toText builder
   in (sql, encoder)
