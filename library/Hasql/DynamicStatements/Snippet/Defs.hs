module Hasql.DynamicStatements.Snippet.Defs where

import Hasql.DynamicStatements.Prelude
import qualified Hasql.Encoders as Encoders


{-|
Dynamically generated statement.
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
