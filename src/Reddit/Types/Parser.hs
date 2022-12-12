module Reddit.Types.Parser
  ( Parser
  , embedIntoAeson
  , module Data.Char
  , module Text.Megaparsec
  , module Text.Megaparsec.Char.Lexer
  ) where

import Data.Char
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

type Parser = Parsec Void Text

embedIntoAeson :: String -> Parser a -> Aeson.Value -> Aeson.Parser a
embedIntoAeson l p = Aeson.withText l $ \s ->
  case parse p l s of
    Left err -> Aeson.parseFail $ errorBundlePretty err
    Right res -> pure res
