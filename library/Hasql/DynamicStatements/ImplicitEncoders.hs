{-# LANGUAGE CPP #-}
module Hasql.DynamicStatements.ImplicitEncoders where

import Hasql.DynamicStatements.Prelude
import Hasql.Encoders
import qualified Data.Aeson as Aeson


class ParamsEncoder a where
  params :: Params a

#define Q(a) a'
#define INSTANCES(VALUE, ENCODER) \
instance ParamsEncoder VALUE where { \
  params = param ENCODER; \
}; \
instance ParamsEncoder [Maybe VALUE] where { \
  params = param (array (dimension Q(foldl) (nullableElement ENCODER))); \
}; \
instance ParamsEncoder [VALUE] where { \
  params = param (array (dimension Q(foldl) (element ENCODER))); \
}; \
instance ParamsEncoder [[VALUE]] where { \
  params = param (array (dimension Q(foldl) (dimension Q(foldl) (element ENCODER)))); \
}; \
instance ParamsEncoder [[Maybe VALUE]] where { \
  params = param (array (dimension Q(foldl) (dimension Q(foldl) (nullableElement ENCODER)))); \
}; \
instance ParamsEncoder (Vector (Maybe VALUE)) where { \
  params = param (array (dimension Q(foldl) (nullableElement ENCODER))); \
}; \
instance ParamsEncoder (Vector VALUE) where { \
  params = param (array (dimension Q(foldl) (element ENCODER))); \
}; \
instance ParamsEncoder (Vector (Vector VALUE)) where { \
  params = param (array (dimension Q(foldl) (dimension Q(foldl) (element ENCODER)))); \
}; \
instance ParamsEncoder (Vector (Vector (Maybe VALUE))) where { \
  params = param (array (dimension Q(foldl) (dimension Q(foldl) (nullableElement ENCODER)))); \
}; \
instance ParamsEncoder (Maybe VALUE) where { \
  params = nullableParam ENCODER; \
}; \
instance ParamsEncoder (Maybe [Maybe VALUE]) where { \
  params = nullableParam (array (dimension Q(foldl) (nullableElement ENCODER))); \
}; \
instance ParamsEncoder (Maybe [VALUE]) where { \
  params = nullableParam (array (dimension Q(foldl) (element ENCODER))); \
}; \
instance ParamsEncoder (Maybe [[VALUE]]) where { \
  params = nullableParam (array (dimension Q(foldl) (dimension Q(foldl) (element ENCODER)))); \
}; \
instance ParamsEncoder (Maybe [[Maybe VALUE]]) where { \
  params = nullableParam (array (dimension Q(foldl) (dimension Q(foldl) (nullableElement ENCODER)))); \
}; \
instance ParamsEncoder (Maybe (Vector (Maybe VALUE))) where { \
  params = nullableParam (array (dimension Q(foldl) (nullableElement ENCODER))); \
}; \
instance ParamsEncoder (Maybe (Vector VALUE)) where { \
  params = nullableParam (array (dimension Q(foldl) (element ENCODER))); \
}; \
instance ParamsEncoder (Maybe (Vector (Vector VALUE))) where { \
  params = nullableParam (array (dimension Q(foldl) (dimension Q(foldl) (element ENCODER)))); \
}; \
instance ParamsEncoder (Maybe (Vector (Vector (Maybe VALUE)))) where { \
  params = nullableParam (array (dimension Q(foldl) (dimension Q(foldl) (nullableElement ENCODER)))); \
}

INSTANCES(Char, char)
INSTANCES(Double, float8)
INSTANCES(Float, float4)
INSTANCES(Int16, int2)
INSTANCES(Int32, int4)
INSTANCES(Int64, int8)
INSTANCES(ByteString, bytea)
INSTANCES(Scientific, numeric)
INSTANCES(Text, text)
INSTANCES(UTCTime, timestamptz)
INSTANCES(Aeson.Value, jsonb)
INSTANCES(UUID, uuid)
INSTANCES(Day, date)
INSTANCES(DiffTime, interval)
INSTANCES(TimeOfDay, time)
INSTANCES(LocalTime, timestamp)
INSTANCES((TimeOfDay, TimeZone), timetz)
INSTANCES((NetAddr IP), inet)

#undef INSTANCES
#undef Q
