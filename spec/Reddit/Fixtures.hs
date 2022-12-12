module Reddit.Fixtures
  ( loadFixture
  , module Data.Yaml
  ) where

import Control.Exception
import Data.Text (Text)
import Data.Yaml
import Test.Hspec

loadFixture :: FromJSON a => FilePath -> IO a
loadFixture fp = do
  let fixturePath = "spec/fixtures" <> fp
  decodeFileEither fixturePath >>= \case
    Right res -> pure (contents res)
    Left err -> do
      throwIO err

data SavedResponse a =
  SavedResponse { url :: Text
                , contents :: a
                }
  deriving (Show, Read, Eq, Ord)

instance FromJSON a => FromJSON (SavedResponse a) where
  parseJSON = withObject "SavedResponse" $ \o -> do
    SavedResponse <$> o .: "url"
                  <*> o .: "body"
