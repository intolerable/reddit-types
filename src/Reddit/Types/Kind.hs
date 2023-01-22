module Reddit.Types.Kind
  ( Kind(..)
  , HasKind(..)
  , Proxy(..)
  , kindOf
  , kindOf'
  , parseKind
  , ensureKind
  , ensuringKind
  , withDataObject
  ) where

import Reddit.Types.Parser

import Control.Monad
import Data.Aeson (FromJSON, Object, (.:))
import Data.Functor
import Data.Proxy
import GHC.Generics
import Data.Aeson.Types qualified as Aeson

data Kind
  = ListingKind
  | MoreKind
  | CommentKind
  | AccountKind
  | LinkKind
  | MessageKind
  | SubredditKind
  | AwardKind
  deriving (Show, Read, Eq, Ord, Generic)

class KnownKind (k :: Kind) where
  kindVal :: Proxy k -> Kind

instance KnownKind 'ListingKind where kindVal Proxy = ListingKind
instance KnownKind 'MoreKind where kindVal Proxy = MoreKind
instance KnownKind 'CommentKind where kindVal Proxy = CommentKind
instance KnownKind 'AccountKind where kindVal Proxy = AccountKind
instance KnownKind 'LinkKind where kindVal Proxy = LinkKind
instance KnownKind 'MessageKind where kindVal Proxy = MessageKind
instance KnownKind 'SubredditKind where kindVal Proxy = SubredditKind
instance KnownKind 'AwardKind where kindVal Proxy = AwardKind

instance FromJSON Kind where
  parseJSON = embedIntoAeson "Kind" parseKind

class KnownKind (KindOf a) => HasKind a where
  type KindOf a :: Kind

kindOf :: forall a . HasKind a => Proxy a -> Kind
kindOf Proxy = kindVal (Proxy :: Proxy (KindOf a))

kindOf' :: forall a . HasKind a => a -> Kind
kindOf' _ = kindOf (Proxy :: Proxy a)

ensureKind :: Kind -> Object -> Aeson.Parser ()
ensureKind expectedKind o = do
  k <- o .: "kind"
  unless (k == expectedKind) $
    Aeson.parseFail $
      "unexpected kind: got " <> show k <> ", expected " <> show expectedKind

ensuringKind :: forall a . HasKind a => (Object -> Aeson.Parser a) -> Object -> Aeson.Parser a
ensuringKind f o = do
  ensureKind (kindOf (Proxy :: Proxy a)) o
  f o

withDataObject :: HasKind a
               => String
               -> (Object -> Aeson.Parser a)
               -> Aeson.Value
               -> Aeson.Parser a
withDataObject s p =
  Aeson.withObject s $ ensuringKind $ \o -> do
    p =<< (o .: "data")

parseKind :: Parser Kind
parseKind = label "kind" $ choice
  [ "Listing" $> ListingKind
  , "more" $> MoreKind
  , "t1" $> CommentKind
  , "t2" $> AccountKind
  , "t3" $> LinkKind
  , "t4" $> MessageKind
  , "t5" $> SubredditKind
  , "t6" $> AwardKind
  ]
