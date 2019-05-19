module Hasql.DynamicStatements.Snippet.Defs where

import Hasql.DynamicStatements.Prelude
import qualified Hasql.Encoders as Encoders


data Snippet = Snippet
  Int {-^ Amount of placeholders -}
  (Seq SnippetChunk)

data SnippetChunk =
  TextSnippetChunk ByteString |
  ParamSnippetChunk (Encoders.Value ())

instance Semigroup Snippet where
  (<>) (Snippet amountOfPlaceholdersL chunksL) (Snippet amountOfPlaceholdersR chunksR) =
    Snippet (amountOfPlaceholdersL + amountOfPlaceholdersR) (chunksL <> chunksR)

instance Monoid Snippet where
  mempty = Snippet 0 mempty
  mappend = (<>)

instance IsString Snippet where
  fromString x = Snippet 0 (pure (TextSnippetChunk (fromString x)))

byteStringSnippet :: ByteString -> Snippet
byteStringSnippet x = Snippet 0 (pure (TextSnippetChunk x))

param :: Encoders.Value param -> param -> Snippet
param encoder param = Snippet 1 (pure (ParamSnippetChunk (param >$ encoder)))

text :: Text -> Snippet
text = param Encoders.text

int8 :: Int64 -> Snippet
int8 = param Encoders.int8

