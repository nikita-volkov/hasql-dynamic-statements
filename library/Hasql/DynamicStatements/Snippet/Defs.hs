module Hasql.DynamicStatements.Snippet.Defs where

import Hasql.DynamicStatements.Prelude
import qualified Hasql.Encoders as Encoders
import qualified Hasql.DynamicStatements.ImplicitEncoders as ImplicitEncoders


newtype Snippet = Snippet (Seq SnippetChunk)

data SnippetChunk =
  TextSnippetChunk ByteString |
  ParamSnippetChunk (Encoders.Params ())

deriving instance Semigroup Snippet
deriving instance Monoid Snippet

instance IsString Snippet where
  fromString x = Snippet (pure (TextSnippetChunk (fromString x)))

sql :: ByteString -> Snippet
sql x = Snippet (pure (TextSnippetChunk x))

param :: ImplicitEncoders.ParamsEncoder param => param -> Snippet
param param = Snippet (pure (ParamSnippetChunk (param >$ ImplicitEncoders.params)))

encoderAndParam :: Encoders.Params param -> param -> Snippet
encoderAndParam encoder param = Snippet (pure (ParamSnippetChunk (param >$ encoder)))
