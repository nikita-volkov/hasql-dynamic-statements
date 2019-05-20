module Hasql.DynamicStatements.Snippet.Defs where

import Hasql.DynamicStatements.Prelude
import qualified Hasql.Encoders as Encoders
import qualified Hasql.DynamicStatements.ImplicitEncoders as ImplicitEncoders


{-|
Dynamically generated statement.
-}
newtype Snippet = Snippet (Seq SnippetChunk)

data SnippetChunk =
  TextSnippetChunk ByteString |
  ParamSnippetChunk (Encoders.Params ())

deriving instance Semigroup Snippet
deriving instance Monoid Snippet

instance IsString Snippet where
  fromString x = Snippet (pure (TextSnippetChunk (fromString x)))

{-|
SQL chunk in ASCII.
-}
sql :: ByteString -> Snippet
sql x = Snippet (pure (TextSnippetChunk x))

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
