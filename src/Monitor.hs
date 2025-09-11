module Monitor where

import qualified Data.Text as Text

data Mode = Mode
  { width :: Int,
    height :: Int
  }

data Info = Info
  { name :: Text.Text,
    mode :: Mode,
    available :: [Mode]
  }
  deriving (Eq, Show)

instance Eq Mode where
  (==) a b = compare a b == EQ

instance Ord Mode where
  compare a b = case compare (width a) (width b) of
    EQ -> compare (height a) (height b)
    GT -> GT
    LT -> LT

instance Show Mode where
  show (Mode w h) = show w ++ "x" ++ show h
