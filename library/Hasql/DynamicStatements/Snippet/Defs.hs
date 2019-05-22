module Hasql.DynamicStatements.Snippet.Defs where

import Hasql.DynamicStatements.Prelude
import qualified Hasql.Encoders as Encoders
import qualified Hasql.DynamicStatements.ImplicitEncoders as ImplicitEncoders


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
param :: ImplicitEncoders.ParamsEncoder param => param -> Snippet
param param = Snippet (pure (ParamSnippetChunk (param >$ ImplicitEncoders.params)))

{-|
Parameter with an explicitly defined encoder.
-}
encoderAndParam :: Encoders.Params param -> param -> Snippet
encoderAndParam encoder param = Snippet (pure (ParamSnippetChunk (param >$ encoder)))
