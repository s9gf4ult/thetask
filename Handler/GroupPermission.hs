module Handler.GroupPermission where

import Import
import qualified Database.Persist     as P
import qualified Yesod.Persist        as P


postGroupPermissionR :: GroupPermissionId -> [Text] -> Handler Html
postGroupPermissionR gpid ["delete"] = do
  gperm <- runDB $ P.get404 gpid
  let gid = groupPermissionGroupId gperm
  runDB $ P.delete gpid
  redirect $ GroupsR $ MPiece gid $ MGroupStd MEmpty
postGroupPermissionR _ _ = notFound
