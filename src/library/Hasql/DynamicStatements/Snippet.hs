module Hasql.DynamicStatements.Snippet
  ( Snippet,

    -- * Execution
    toSql,
    toStatement,
    toSession,
    toPipeline,

    -- * Construction
    param,
    encoderAndParam,
    sql,
  )
where

import Hasql.Decoders qualified as Decoders
import Hasql.DynamicStatements.Prelude
import Hasql.Encoders qualified as Encoders
import Hasql.Implicits.Encoders qualified as Encoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
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
-- 'Hasql.DynamicStatements.Snippet.toStatement' or directly execute it in
-- 'Hasql.Session.Session' using
-- 'Hasql.DynamicStatements.Snippet.toSession'.
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

-- |
-- Compile a snippet into SQL text with placeholders.
toSql :: Snippet -> Text
toSql snippet = fst (compile snippet)

-- |
-- Construct a statement dynamically, specifying the parameters in-place
-- in the declaration of snippet and providing a result decoder and
-- specifying whether the statement should be prepared.
--
-- The injection of the parameters is handled automatically,
-- generating parametric statements with binary encoders under the hood.
--
-- This is useful when the SQL of your statement depends on the parameters.
-- Here's an example:
--
-- @
-- selectSubstring :: Text -> Maybe Int32 -> Maybe Int32 -> 'Statement' () Text
-- selectSubstring string from to = let
--   snippet =
--     "select substring(" <> Snippet.'param' string <>
--     foldMap (mappend " from " . Snippet.'param') from <>
--     foldMap (mappend " for " . Snippet.'param') to <>
--     ")"
--   decoder = Decoders.'Decoders.singleRow' (Decoders.'Decoders.column' (Decoders.'Decoders.nonNullable' Decoders.'Decoders.text'))
--   in 'toStatement' snippet decoder
-- @
--
-- Without the Snippet API you would have had to implement the same functionality thus:
--
-- @
-- selectSubstring' :: Text -> Maybe Int32 -> Maybe Int32 -> 'Statement' () Text
-- selectSubstring' string from to = let
--   sql = case (from, to) of
--     (Just _, Just _) -> "select substring($1 from $2 to $3)"
--     (Just _, Nothing) -> "select substring($1 from $2)"
--     (Nothing, Just _) -> "select substring($1 to $2)"
--     (Nothing, Nothing) -> "select substring($1)"
--   encoder =
--     Encoders.'Encoders.param' (string '>$' Encoders.'Encoders.text') '<>'
--     foldMap (\\ x -> Encoders.'Encoders.param' (x '>$' Encoders.'Encoders.int8')) from '<>'
--     foldMap (\\ x -> Encoders.'Encoders.param' (x '>$' Encoders.'Encoders.int8')) to
--   decoder = Decoders.'Decoders.singleRow' (Decoders.'Decoders.column' (Decoders.'Decoders.nonNullable' Decoders.'Decoders.text'))
--   in Statement.'Statement.preparable' sql encoder decoder
-- @
--
-- As you can see, the Snippet API abstracts over placeholders and
-- matching encoder generation, thus also protecting you from all sorts of related bugs.
toStatement :: Snippet -> Decoders.Result result -> Statement.Statement () result
toStatement snippet =
  let (sql, encoder) = compile snippet
   in Statement.unpreparable sql encoder

-- |
-- Execute in @Session.Session@, providing a result decoder.
--
-- This is merely a shortcut defined thus:
--
-- @
-- toSession snippet decoder =
--   Session.statement () (Snippet.toStatement snippet decoder)
-- @
toSession :: Snippet -> Decoders.Result result -> Session.Session result
toSession snippet decoder =
  Session.statement () (toStatement snippet decoder)

-- |
-- Execute in @Pipeline.Pipeline@, providing a result decoder.
--
-- This is merely a shortcut defined thus:
--
-- @
-- toPipeline snippet decoder =
--   Pipeline.statement () (Snippet.toStatement snippet decoder)
-- @
toPipeline :: Snippet -> Decoders.Result result -> Pipeline.Pipeline result
toPipeline snippet decoder =
  Pipeline.statement () (toStatement snippet decoder)
