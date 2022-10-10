module Reddit.Types.Parser
  ( Parser
  , module Data.Char
  , module Text.Megaparsec
  , module Text.Megaparsec.Char.Lexer
  ) where

import Data.Char
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text
