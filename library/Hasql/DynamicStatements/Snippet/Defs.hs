module Hasql.DynamicStatements.Snippet.Defs where

import Hasql.DynamicStatements.Prelude
import qualified Hasql.Encoders as Encoders


newtype Snippet = Snippet (Seq SnippetChunk)

data SnippetChunk =
  TextSnippetChunk ByteString |
  ParamSnippetChunk (Encoders.Value ())

deriving instance Semigroup Snippet
deriving instance Monoid Snippet

instance IsString Snippet where
  fromString x = Snippet (pure (TextSnippetChunk (fromString x)))

byteStringSnippet :: ByteString -> Snippet
byteStringSnippet x = Snippet (pure (TextSnippetChunk x))

param :: Encoders.Value param -> param -> Snippet
param encoder param = Snippet (pure (ParamSnippetChunk (param >$ encoder)))

text :: Text -> Snippet
text = param Encoders.text

int8 :: Int64 -> Snippet
int8 = param Encoders.int8

