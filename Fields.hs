module Fields where

import Text.Blaze
import Text.Blaze.Internal
import Data.Monoid ((<>))
import Data.Text (pack, unpack)
import Data.Typeable
import Database.Persist
import Database.Persist.Sql
import Prelude

data PermissionValues = PVAdmin -- Permit manage groups and permissions
                      | PVDelegateAdmin -- Permit delegate admin rights to other groups
                      deriving (Typeable, Show, Read, Eq)

instance PersistField PermissionValues where
  toPersistValue a = PersistText $ pack $ show a
  fromPersistValue (PersistText t) = case (reads $ unpack t) of
    ((a, ""):_) -> Right a
    _ -> Left $ "Could not convert " <> t <> " to PermissionValues"
  fromPersistValue _ = Left "Incorrect type to convert to PermissionValues"

instance PersistFieldSql  PermissionValues where
  sqlType _ = SqlString

instance ToMarkup PermissionValues where
  toMarkup PVAdmin = text "Admin rights"
  toMarkup PVDelegateAdmin = text "Delegate admin rights"
