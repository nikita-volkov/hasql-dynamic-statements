module Hasql.DynamicStatements.ImplicitEncoders where

import Hasql.DynamicStatements.Prelude
import Hasql.Encoders


class ParamsEncoder a where
  params :: Params a

instance ParamsEncoder Text where
  params = param text

instance ParamsEncoder [Maybe Text] where
  params = param (array (dimension foldl' (nullableElement text)))

instance ParamsEncoder [Text] where
  params = param (array (dimension foldl' (element text)))

instance ParamsEncoder [[Text]] where
  params = param (array (dimension foldl' (dimension foldl' (element text))))

instance ParamsEncoder [[Maybe Text]] where
  params = param (array (dimension foldl' (dimension foldl' (nullableElement text))))

instance ParamsEncoder (Maybe Text) where
  params = nullableParam text

instance ParamsEncoder (Maybe [Maybe Text]) where
  params = nullableParam (array (dimension foldl' (nullableElement text)))

instance ParamsEncoder (Maybe [Text]) where
  params = nullableParam (array (dimension foldl' (element text)))

instance ParamsEncoder (Maybe [[Text]]) where
  params = nullableParam (array (dimension foldl' (dimension foldl' (element text))))

instance ParamsEncoder (Maybe [[Maybe Text]]) where
  params = nullableParam (array (dimension foldl' (dimension foldl' (nullableElement text))))
