module Hasql.DynamicStatements.Snippet.Defs where

import Hasql.DynamicStatements.Prelude
import qualified Hasql.Encoders as Encoders


{-|
Composable SQL snippet with parameters injected.
Abstracts over placeholders and matching of encoders.

It has an instance of `IsString`, so having the @OverloadedStrings@ extension enabled
you can use string literals to construct it from string.

Here's an example:

@
selectSubstring :: Text -> Maybe Int32 -> Maybe Int32 -> 'Snippet'
selectSubstring string from to =
  "select substring(" <> 'param' string <>
  'foldMap' (\\ x -> " from " <> 'param' x) from <>
  'foldMap' (\\ x -> " for " <> 'param' x) to <>
  ")"
@

Having a decoder you can lift it into 'Hasql.Statement.Statement' using
'Hasql.DynamicStatements.Statement.dynamicallyParameterized' or directly execute it in
'Hasql.Session.Session' using
'Hasql.DynamicStatements.Session.snippet'.
-}
newtype Snippet = Snippet (Seq SnippetChunk)

data SnippetChunk =
  StringSnippetChunk ByteString |
  ParamSnippetChunk (Encoders.Params ())

deriving instance Semigroup Snippet
deriving instance Monoid Snippet

instance IsString Snippet where
  fromString x = Snippet (pure (StringSnippetChunk (fromString x)))

{-|
SQL chunk in ASCII.
-}
sql :: ByteString -> Snippet
sql x = Snippet (pure (StringSnippetChunk x))

{-|
Parameter encoded using an implicitly derived encoder from the type.
-}
param :: Default (Encoders.Value param) => param -> Snippet
param = encoderAndParam def

{-|
Nullable parameter encoded using an implicitly derived encoder from the type.
-}
nullableParam :: Default (Encoders.Value param) => Maybe param -> Snippet
nullableParam = encoderAndNullableParam def

{-|
Parameter with an explicitly defined encoder.
-}
encoderAndParam :: Encoders.Value param -> param -> Snippet
encoderAndParam = paramsEncoderAndParam . Encoders.param

{-|
Nullable parameter with an explicitly defined encoder.
-}
encoderAndNullableParam :: Encoders.Value param -> Maybe param -> Snippet
encoderAndNullableParam =  paramsEncoderAndParam . Encoders.nullableParam

paramsEncoderAndParam :: Encoders.Params param -> param -> Snippet
paramsEncoderAndParam encoder param = Snippet (pure (ParamSnippetChunk (param >$ encoder)))
