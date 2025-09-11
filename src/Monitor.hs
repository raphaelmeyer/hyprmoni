module Monitor where

import qualified Data.Text as Text

data Info = Info
  { name :: Text.Text,
    mode :: Text.Text,
    available :: [Text.Text]
  }
  deriving (Eq, Show)
