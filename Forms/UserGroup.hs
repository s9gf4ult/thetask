module Forms.UserGroup where

import Import

userGroupCreateForm :: Maybe UserId -> Maybe GroupId -> Form UserGroup
userGroupCreateForm uid gid = renderDivs
                              $ UserGroup
                              <$> areq hiddenField "" uid
                              <*> areq hiddenField "" gid
