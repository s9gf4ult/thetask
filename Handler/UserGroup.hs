module Handler.UserGroup where

import Import

postUserGroupR :: UserGroupId -> [Text] -> Handler Html
postUserGroupR ugid ["delete"] = do
  userGroup <- runDB $ get404 ugid
  let gid = userGroupGroupId userGroup
  runDB $ delete ugid
  redirect $ GroupR gid []
postUserGroupR _ _ = notFound
