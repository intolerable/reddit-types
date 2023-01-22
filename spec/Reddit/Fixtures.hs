module Reddit.Fixtures
  ( loadFixture
  , loadFixtures
  , loadFixtures_
  , Proxy(..)
  , module Data.Yaml
  ) where

import Control.Exception
import Control.Monad
import Data.Proxy
import Data.Text (Text)
import Data.Yaml

loadFixture :: forall a . FromJSON a => FilePath -> IO a
loadFixture fp = do
  let fixturePath = "spec/fixtures" <> fp
  decodeFileEither fixturePath >>= \case
    Right res -> pure (body res)
    Left err -> do
      throwIO err

loadFixtures :: forall a . FromJSON a => [FilePath] -> IO [a]
loadFixtures = mapM loadFixture

loadFixtures_ :: forall a . FromJSON a => Proxy a -> [FilePath] -> IO ()
loadFixtures_ Proxy = void . loadFixtures @a

data SavedResponse a =
  SavedResponse { url :: Text
                , body :: a
                }
  deriving (Show, Read, Eq, Ord)

instance FromJSON a => FromJSON (SavedResponse a) where
  parseJSON = withObject "SavedResponse" $ \o -> do
    SavedResponse <$> o .: "url"
                  <*> o .: "body"
