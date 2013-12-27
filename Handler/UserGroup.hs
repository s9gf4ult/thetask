module Handler.UserGroup where

import Import

deleteUserGroupR :: UserGroupId -> Handler Html
deleteUserGroupR ugid = do
  userGroup <- runDB $ get404 ugid
  let gid = userGroupGroupId userGroup
  runDB $ deleteWhere [UserGroupId ==. ugid]
  redirect $ GroupR gid []
