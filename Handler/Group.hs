{-# LANGUAGE
  ScopedTypeVariables
  #-}
module Handler.Group where

import Control.Monad (forM)
import Database.Esqueleto
import Forms.Group
import Forms.GroupPermission
import Forms.UserGroup
import Import
import qualified Database.Persist     as P
import qualified Yesod.Persist        as P

getGroupR :: GroupId -> [Text] -> Handler Html
getGroupR gid [] = do
  group <- runDB $ P.get404 gid
  users <- runDB
           $ select
           $ from $ \(user `InnerJoin` userGroup `InnerJoin` group) -> do
             on (user ^. UserId ==. userGroup ^. UserGroupUserId)
             on (userGroup ^. UserGroupGroupId ==. group ^. GroupId)
             where_ (group ^. GroupId ==. (val gid))
             orderBy [asc (user ^. UserEmail)]
             return (user, userGroup)
  permissions <- runDB $ P.selectList [GroupPermissionGroupId P.==. gid] []
  defaultLayout $(widgetFile "Group/show")
getGroupR gid ["edit"] = do
  group <- runDB $ P.get404 gid
  (widget, enctype) <- generateFormPost $ groupForm $ Just group
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Group/edit")
getGroupR gid ["attach_user"] = do
  group <- runDB $ P.get404 gid
  users <- runDB
           $ select
           $ from $ \user -> do
             where_ $ notExists $ do
               from $ \userGroup -> do
                 where_ $ (userGroup ^. UserGroupGroupId ==. (val gid)) &&. (userGroup ^. UserGroupUserId ==. user ^. UserId)
               return ()
             return user
  widgets <- forM users $ \(Entity uid user) -> do
    (widget, enctype) <- generateFormPost $ userGroupCreateForm (Just uid) (Just gid)
    return (widget, enctype, user)
  defaultLayout $(widgetFile "Group/attach_user")
getGroupR gid ["new_permission"] = do
  group <- runDB $ P.get404 gid
  (widget, enctype) <- generateFormPost $ newGroupPermission $ Just gid
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Group/new_permission")
getGroupR _ _ = notFound



postGroupR :: GroupId -> [Text] -> Handler Html
postGroupR gid [] = do
  g <- runDB $ P.get404 gid
  ((res, widget), enctype) <- runFormPost $ groupForm Nothing
  case res of
    FormSuccess group -> do
      let gname = groupName group
      oldg <- runDB $ P.getBy $ UniqueGroup gname
      case oldg of
        Nothing -> do
          runDB $ P.update gid [P.Update GroupName gname P.Assign]
          redirect $ GroupR gid []
        Just _ -> do
          let fails :: [Text] = ["There is already group with such name"]
          defaultLayout $(widgetFile "Group/edit")
    FormFailure fails -> do
      let group = g
      defaultLayout $(widgetFile "Group/edit")
postGroupR _ _ = notFound

deleteGroupR :: GroupId -> [Text] -> Handler Html
deleteGroupR = error "Not yet implemented: deleteGroupR"
