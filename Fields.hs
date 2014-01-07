module Fields where

import Control.Applicative ((<|>), (*>))
import Data.Attoparsec.Text
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable
import Database.Persist
import Database.Persist.Sql
import Prelude
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Internal (text)

data PermissionValues = PVAdmin -- Permit manage groups and permissions
                      | PVDelegateAdmin -- Permit delegate admin rights to other groups
                      | PVReadEmail Text
                      | PVWriteEmail Text
                      | PVReserveRooms
                      | PVReadFinance Text
                      | PVUnknown
                      deriving (Typeable, Show, Read, Eq, Ord)

instance ToMarkup PermissionValues where
  toMarkup PVAdmin = text "Admin rights"
  toMarkup PVDelegateAdmin = text "Delegate admin rights"
  toMarkup (PVReadEmail t) = text $ "Read email " <> t
  toMarkup (PVWriteEmail t) = text $ "Write to email" <> t
  toMarkup PVReserveRooms = text "Reserve conference rooms"
  toMarkup (PVReadFinance t) = text $ "Read financial reports from " <> t
  toMarkup PVUnknown = text "Unknown permission"

instance PersistField PermissionValues where
  toPersistValue PVAdmin = PersistText "admin"
  toPersistValue PVDelegateAdmin = PersistText "delegate admin"
  toPersistValue (PVReadEmail t) = PersistText $ "read email " <> t
  toPersistValue (PVWriteEmail t) = PersistText $ "write email " <> t
  toPersistValue PVReserveRooms = PersistText "reserve rooms"
  toPersistValue (PVReadFinance t) = PersistText $ "read finance " <> t
  toPersistValue PVUnknown = PersistText ""

  fromPersistValue (PersistText t) = case (parseOnly permissionParser t) of
    Right a -> Right a
    Left _ -> Right PVUnknown
  fromPersistValue _ = Left "Incorrect type to convert to PermissionValues"

instance PersistFieldSql  PermissionValues where
  sqlType _ = SqlString

permissionParser :: Parser PermissionValues
permissionParser = (string "admin" *> return PVAdmin)
                   <|> (string "delegate admin" *> return PVDelegateAdmin)
                   <|> readEmailParser
                   <|> writeEmailParser
                   <|> (string "reserve rooms" *> return PVReserveRooms)
                   <|> readFinance

readEmailParser :: Parser PermissionValues
readEmailParser = do
  _ <- string "read email"
  skipSpace
  e <- takeText
  return $ PVReadEmail e

writeEmailParser :: Parser PermissionValues
writeEmailParser = do
  _ <- string "write email"
  skipSpace
  e <- takeText
  return $ PVWriteEmail e

readFinance :: Parser PermissionValues
readFinance = do
  _ <- string "read finance"
  skipSpace
  f <- takeText
  return $ PVReadFinance f
