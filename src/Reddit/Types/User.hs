module Reddit.Types.User where

import Data.Aeson

data User = User
  deriving (Show, Read, Eq, Ord)

instance FromJSON User where
  parseJSON = withObject "User" $ \_ -> do
    pure User
