module Reddit.Types.Thing
  ( ThingId(..)
  , parseThingId
  , FullName(..)
  , parseFullName
  , HasFullName(..)
  ) where

import Reddit.Types.Kind
import Reddit.Types.Parser

import Control.Monad
import Data.Aeson (FromJSON(..))
import Data.Kind
import Data.Text (Text)
import GHC.Exts
import qualified Data.CaseInsensitive as CI

newtype ThingId = ThingId Text
  deriving (Show, Read, IsString)

instance Eq ThingId where
  ThingId a == ThingId b = CI.mk a == CI.mk b

instance Ord ThingId where
  ThingId a `compare` ThingId b = CI.mk a `compare` CI.mk b

instance FromJSON ThingId where
  parseJSON = embedIntoAeson "ThingId" parseThingId

parseThingId :: Parser ThingId
parseThingId =
  ThingId <$> takeWhile1P (Just "thingId") isAlphaNum

data FullName a = FullName Kind ThingId
  deriving (Show, Read, Eq, Ord)

instance FromJSON (FullName a) where
  parseJSON = embedIntoAeson "FullName" parseFullName

parseFullName :: Parser (FullName a)
parseFullName = label "FullName" $ do
  k <- parseKind
  void "_"
  t <- parseThingId
  pure $ FullName k t

class HasFullName (a :: Type) where
  type FullNameTypeOf a :: Type
  type FullNameTypeOf a = a

  fullName :: a -> FullName (FullNameTypeOf a)

