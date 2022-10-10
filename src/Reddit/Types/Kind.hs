module Reddit.Types.Kind
  ( Kind(..)
  , HasKind(..)
  , parseKind
  , pattern Comment
  , pattern Account
  , pattern Link
  , pattern Message
  , pattern Subreddit
  , pattern Award
  ) where

import Reddit.Types.Parser

import Data.Aeson (FromJSON, Object, (.:))
import Control.Monad
import Data.Proxy
import qualified Data.Aeson.Types as Aeson

newtype Kind = Kind Int
  deriving (Show, Read, Eq, Ord)

instance FromJSON Kind where
  parseJSON = withString "Kind" $ \s -> _ s

class HasKind a where
  kindOf :: Proxy a -> Kind

ensureKind :: Kind -> Object -> Aeson.Parser ()
ensureKind expectedKind o = do
  k <- o .: "kind"
  unless (k == expectedKind) $
    Aeson.parseFail $
      "unexpected kind: got " <> show k <> ", expected " <> show expectedKind

parseKind :: Parser Kind
parseKind = label "kind" do
  void "t"
  d <- decimal <?> "decimal"
  guard (d `elem` knownKinds) <?> "knownKinds"
  pure $ Kind d

knownKinds :: [Int]
knownKinds = [1,2,3,4,5,6]

pattern Comment :: Kind
pattern Comment = Kind 1

pattern Account :: Kind
pattern Account = Kind 2

pattern Link :: Kind
pattern Link = Kind 3

pattern Message :: Kind
pattern Message = Kind 4

pattern Subreddit :: Kind
pattern Subreddit = Kind 5

pattern Award :: Kind
pattern Award = Kind 6

