module Reddit.Types.Thing
  ( ThingId(..)
  , parseThingId
  ) where

import Reddit.Types.Parser

import Data.Text (Text)
import qualified Data.CaseInsensitive as CI

newtype ThingId = ThingId Text
  deriving (Show, Read)

instance Eq ThingId where
  ThingId a == ThingId b =
    CI.mk a == CI.mk b

instance Ord ThingId where
  compare (ThingId a) (ThingId b) = compare (CI.mk a) (CI.mk b)

parseThingId :: Parser ThingId
parseThingId =
  ThingId <$> takeWhile1P (Just "thingId") isAlphaNum
