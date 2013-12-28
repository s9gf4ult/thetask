module Handler.UserGroup where

import Import
import qualified Database.Persist     as P
import qualified Yesod.Persist        as P


postUserGroupR :: UserGroupId -> [Text] -> Handler Html
postUserGroupR ugid ["delete"] = do
  userGroup <- runDB $ P.get404 ugid
  let gid = userGroupGroupId userGroup
  runDB $ P.delete ugid
  redirect $ GroupR gid []
postUserGroupR _ _ = notFound
