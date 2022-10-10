module Reddit.Fixtures where

import Control.Exception
import Data.Aeson
import Test.Hspec

loadFixture :: FromJSON a => FilePath -> IO a
loadFixture fp = do
  let fixturePath = "spec/fixtures" <> fp
  eitherDecodeFileStrict' fixturePath >>= \case
    Right res -> pure res
    Left err -> do
      expectationFailure $
        "Failed to parse " <> fixturePath <> " with error: " <> err
      throwIO $ AssertionFailed "failed to parse JSON file in loadFixture"

