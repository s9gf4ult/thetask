module Handler.GroupPermission where

import Import

postGroupPermissionR :: GroupPermissionId -> [Text] -> Handler Html
postGroupPermissionR gpid ["delete"] = do
  gperm <- runDB $ get404 gpid
  let gid = groupPermissionGroupId gperm
  runDB $ delete gpid
  redirect $ GroupR gid []
postGroupPermissionR _ _ = notFound
