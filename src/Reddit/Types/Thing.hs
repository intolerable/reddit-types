module Reddit.Types.Thing
  ( ThingId(..)
  , parseThingId
  -- , AThingId(..)
  -- , parseAThingId
  , FullName(..)
  , parseFullName
  -- , AFullName(..)
  -- , parseAFullName
  , HasFullName(..)
  ) where

import Reddit.Types.Kind
import Reddit.Types.Parser

import Control.Monad
import Data.Aeson (FromJSON(..))
import Data.Kind
import Data.Text (Text)
import GHC.Exts
import Servant.API
import qualified Data.CaseInsensitive as CI

newtype ThingId a = ThingId Text
  deriving (Show, Read, IsString, ToHttpApiData)

instance Eq (ThingId a) where
  ThingId a == ThingId b = CI.mk a == CI.mk b

instance Ord (ThingId a) where
  ThingId a `compare` ThingId b = CI.mk a `compare` CI.mk b

instance FromJSON (ThingId a) where
  parseJSON = embedIntoAeson "ThingId" (parseThingId <* eof)

parseThingId :: Parser (ThingId a)
parseThingId = label "ThingId" do
  AThingId t <- parseAThingId
  pure $ ThingId t

newtype AThingId = AThingId Text
  deriving (Show, Read, IsString, ToHttpApiData)

instance Eq AThingId where
  AThingId a == AThingId b = CI.mk a == CI.mk b

instance Ord AThingId where
  AThingId a `compare` AThingId b = CI.mk a `compare` CI.mk b

instance FromJSON AThingId where
  parseJSON = embedIntoAeson "AThingId" (parseAThingId <* eof)

parseAThingId :: Parser AThingId
parseAThingId = label "AThingId" do
  AThingId <$> takeWhile1P (Just "thingId") isAlphaNum

_fromThingId :: ThingId a -> AThingId
_fromThingId (ThingId t) = AThingId t

data FullName a = FullName (ThingId a)
  deriving (Show, Read, Eq, Ord)

instance HasKind a => FromJSON (FullName a) where
  parseJSON = embedIntoAeson "FullName" (parseFullName <* eof)

parseFullName :: forall a . HasKind a => Parser (FullName a)
parseFullName = label "FullName" $ do
  k <- parseKind
  guard $ k == kindOf (Proxy :: Proxy a)
  void "_"
  t <- parseThingId
  pure $ FullName t

data AFullName = AFullName Kind AThingId
  deriving (Show, Read, Eq, Ord)

instance FromJSON AFullName where
  parseJSON = embedIntoAeson "AFullName" (parseAFullName <* eof)

parseAFullName :: Parser AFullName
parseAFullName = label "FullName" $ do
  k <- parseKind
  void "_"
  t <- parseAThingId
  pure $ AFullName k t

_fromFullName :: forall a . HasKind a => FullName a -> AFullName
_fromFullName (FullName t) = AFullName (kindOf (Proxy :: Proxy a)) (_fromThingId t)

class HasKind a => HasFullName (a :: Type) where
  fullName :: a -> FullName a

