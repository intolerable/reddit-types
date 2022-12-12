module Reddit.Types.User where

import Reddit.Types.Kind
import Reddit.Types.Thing

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.CaseInsensitive as CI

data User =
  User { username :: Username
       , userId :: ThingId
       , created :: UTCTime
       , createdUtc :: UTCTime
       , linkKarma :: Integer
       , commentKarma :: Integer
       , totalKarma :: Integer
       , isFriend :: Bool
       , isBlocked :: Bool
       , isMod :: Bool
       , isGold :: Bool
       , isEmployee :: Bool
       , verified :: Bool
       , hasVerifiedEmail :: Bool
       }
  deriving (Show, Read, Eq, Ord)

instance HasKind User where
  kindOf Proxy = Reddit.Types.Kind.Account

instance HasFullName User where
  fullName = FullName <$> kindOf' <*> userId

instance FromJSON User where
  parseJSON = withObject "User" $ ensuringKind $ \o -> do
    d <- o .: "data"
    username <- d .: "name"
    userId <- d .: "id"
    created <- posixSecondsToUTCTime <$> d .: "created"
    createdUtc <- posixSecondsToUTCTime <$> d .: "created_utc"
    linkKarma <- d .: "link_karma"
    commentKarma <- d .: "comment_karma"
    totalKarma <- d .: "total_karma"
    isFriend <- d .: "verified"
    isBlocked <- d .: "is_friend"
    isMod <- d .: "is_blocked"
    isGold <- d .: "is_mod"
    isEmployee <- d .: "is_gold"
    verified <- d .: "is_employee"
    hasVerifiedEmail <- d .: "has_verified_email"
    pure User{..}

newtype Username = Username Text
  deriving (Show, Read, FromJSON)

instance Eq Username where
  Username x == Username y = CI.mk x == CI.mk y

instance Ord Username where
  Username x `compare` Username y = CI.mk x `compare` CI.mk y

data Me =
  Me { user :: User
     , numFriends :: Int
     , over18 :: Bool
     }
  deriving (Show, Read, Eq, Ord)

instance FromJSON Me where
  parseJSON = withObject "Me" $ \o -> do
    d <- o .: "data"
    user <- parseJSON (Object o)
    numFriends <- d .: "num_friends"
    over18 <- d .: "over_18"
    pure Me{..}

instance HasKind Me where
  kindOf Proxy = Account

instance HasFullName Me where
  type FullNameTypeOf Me = User
  fullName = fullName . user


