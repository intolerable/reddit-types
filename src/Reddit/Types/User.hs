module Reddit.Types.User where

import Reddit.Types.Kind
import Reddit.Types.Thing
import Reddit.Types.ExtraFields

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Fmt
import GHC.Generics
import Numeric.Natural
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI

newtype Username = Username (CI Text)
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Username where
  parseJSON = fmap (Username . CI.mk) . parseJSON

instance Buildable Username where
  build (Username ciText) = build (CI.original ciText)

pattern U :: CI Text -> Username
pattern U a = Username a

data User' extra =
  User' { username :: Username
        , userId :: ThingId (User' extra)
        , created :: UTCTime
        , createdUtc :: UTCTime
        , linkKarma :: Natural
        , commentKarma :: Natural
        , totalKarma :: Natural
        , isFriend :: Bool
        , isBlocked :: Bool
        , isMod :: Bool
        , isGold :: Bool
        , isEmployee :: Bool
        , verified :: Bool
        , hasVerifiedEmail :: Bool
        , extraFields :: extra
        }
  deriving (Show, Read, Eq, Ord, Generic)

type User = User' (ExtraFields '[] 'AccountKind)
type UserF fs = User' (ExtraFields fs 'AccountKind)

instance HasKind (User' fs) where
  type KindOf (User' fs) = 'AccountKind

instance HasFullName (User' fs) where
  fullName = FullName <$> userId

instance FromJSON extra => FromJSON (User' extra) where
  parseJSON = withDataObject "User" $ \d -> do
    username <- d .: "name"
    userId <- d .: "id"
    created <- posixSecondsToUTCTime <$> d .: "created"
    createdUtc <- posixSecondsToUTCTime <$> d .: "created_utc"
    linkKarma <- d .: "link_karma"
    commentKarma <- d .: "comment_karma"
    totalKarma <- d .: "total_karma"
    isFriend <- d .: "is_friend"
    isBlocked <- d .: "is_blocked"
    isMod <- d .: "is_mod"
    isGold <- d .: "is_gold"
    isEmployee <- d .: "is_employee"
    verified <- d .: "verified"
    hasVerifiedEmail <- d .: "has_verified_email"
    extraFields <- parseJSON (Object d)
    pure User'{..}
