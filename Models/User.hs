module Models.User where

import Prelude hiding (head, init, last, readFile, tail, writeFile)
import Database.Esqueleto
import Model
import Fields
import Yesod.Core
import Yesod.Persist.Core
import qualified Data.Set as S

userPermissions :: (YesodPersist site, YesodPersistBackend site ~ SqlPersistT) => UserId -> HandlerT site IO (S.Set PermissionValues)
userPermissions uid = do
  perms <- runDB $ selectDistinct
           $ from $ \(userGroup `InnerJoin` group `InnerJoin` groupPermission) -> do
             on (userGroup ^. UserGroupGroupId ==. group ^. GroupId)
             on (groupPermission ^. GroupPermissionGroupId ==. group ^. GroupId)
             where_ (userGroup ^. UserGroupUserId ==. val uid)
             return groupPermission
  return $ S.fromList $ map (groupPermissionValue . entityVal) perms
