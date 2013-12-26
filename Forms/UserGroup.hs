module Forms.UserGroup where

import Import

userGroupCreateForm :: UserId -> GroupId -> Form UserGroup
userGroupCreateForm uid gid = renderDivs
                              $ UserGroup
                              <$> areq hiddenField "user id" (Just uid)
                              <*> areq hiddenField "group id" (Just gid)
