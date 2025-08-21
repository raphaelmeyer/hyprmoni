module Json (KeyValues, Json (..), parse) where

import Control.Applicative ((<|>))
import qualified Data.Char as Char
import qualified Data.Functor as Functor
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Text.ParserCombinators.ReadP as ReadP

type KeyValues = Map.Map Text.Text Json

data Json
  = Array [Json]
  | Object KeyValues
  | String Text.Text
  | Boolean Bool
  | Number Double
  | Null
  deriving (Eq, Show)

parse :: Text.Text -> Maybe Json
parse input = case ReadP.readP_to_S json $ Text.unpack input of
  [(result, _)] -> Just result
  _ -> Nothing

json :: ReadP.ReadP Json
json =
  array
    <|> object
    <|> string
    <|> boolean
    <|> number
    <|> Json.null

array :: ReadP.ReadP Json
array = do
  ReadP.skipSpaces <* ReadP.char '['
  items <- ReadP.sepBy json comma
  ReadP.skipSpaces <* ReadP.char ']'
  pure $ Array items

object :: ReadP.ReadP Json
object = do
  ReadP.skipSpaces <* ReadP.char '{'
  items <- ReadP.sepBy keyValue comma
  ReadP.skipSpaces <* ReadP.char '}'
  pure $ Object (Map.fromList items)

string :: ReadP.ReadP Json
string = String <$> stringValue

boolean :: ReadP.ReadP Json
boolean = do
  ReadP.skipSpaces
  true <|> false

number :: ReadP.ReadP Json
number = do
  ReadP.skipSpaces
  sign <- ReadP.option id (negate <$ ReadP.char '-')
  intPart <- ReadP.munch1 Char.isDigit
  fracPart <- ReadP.option "" $ do
    (:) <$> ReadP.char '.' <*> ReadP.munch1 Char.isDigit
  let value = read (intPart ++ fracPart)
  pure $ Number (sign value)

null :: ReadP.ReadP Json
null = Null <$ ReadP.skipSpaces <* ReadP.string "null"

keyValue :: ReadP.ReadP (Text.Text, Json)
keyValue = do
  key <- identifier
  ReadP.skipSpaces <* ReadP.char ':'
  value <- json
  pure (key, value)

identifier :: ReadP.ReadP Text.Text
identifier = do
  ReadP.skipSpaces
  Functor.void $ ReadP.char '"'
  content <- ReadP.many (ReadP.satisfy Char.isAlphaNum)
  Functor.void $ ReadP.char '"'
  pure $ Text.pack content

stringValue :: ReadP.ReadP Text.Text
stringValue = do
  ReadP.skipSpaces
  Functor.void $ ReadP.char '"'
  content <- ReadP.many (ReadP.satisfy (/= '"'))
  Functor.void $ ReadP.char '"'
  pure $ Text.pack content

true :: ReadP.ReadP Json
true = Boolean True <$ ReadP.skipSpaces <* ReadP.string "true"

false :: ReadP.ReadP Json
false = Boolean True <$ ReadP.skipSpaces <* ReadP.string "false"

comma :: ReadP.ReadP ()
comma = ReadP.skipSpaces <* ReadP.char ','
