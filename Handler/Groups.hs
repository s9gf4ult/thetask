{-# LANGUAGE
  ScopedTypeVariables
  #-}

module Handler.Groups where

import Control.Monad (forM)
import Database.Esqueleto
import Forms.Group
import Forms.GroupPermission
import Forms.UserGroup
import Import
import qualified Database.Persist     as P
import qualified Yesod.Persist        as P


getGroupsR :: Handler Html
getGroupsR = do
  groups <- runDB $ P.selectList [] []
  defaultLayout $(widgetFile "Groups/index")

getGroupsNewR :: Handler Html
getGroupsNewR = _getGroupsNewR []

_getGroupsNewR :: [Text] -> Handler Html
_getGroupsNewR fails = do
  (widget, enctype) <- generateFormPost $ groupForm Nothing
  defaultLayout $(widgetFile "Groups/new")

postGroupsNewR :: Handler Html
postGroupsNewR = do
  ((res, widget), enctype) <- runFormPost $ groupForm Nothing
  case res of
    FormSuccess group -> do
      oldg <- runDB $ P.getBy $ UniqueGroup $ groupName group
      case oldg of
        Nothing -> do
          gid <- runDB $ P.insert group
          redirect $ GroupR gid
        Just g -> do
          _getGroupsNewR ["There is one group with such name"]
    FormFailure fails -> defaultLayout $(widgetFile "Groups/new")
    _ -> redirect GroupsR

getGroupR :: GroupId -> Handler Html
getGroupR gid = do
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
  defaultLayout $(widgetFile "Groups/show")

getGroupEditR :: GroupId -> Handler Html
getGroupEditR gid = do
  group <- runDB $ P.get404 gid
  (widget, enctype) <- generateFormPost $ groupForm $ Just group
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Groups/edit")

postGroupEditR :: GroupId -> Handler Html
postGroupEditR gid = do
  g <- runDB $ P.get404 gid
  ((res, widget), enctype) <- runFormPost $ groupForm Nothing
  case res of
    FormSuccess group -> do
      let gname = groupName group
      oldg <- runDB $ P.getBy $ UniqueGroup gname
      case oldg of
        Nothing -> do
          runDB $ P.update gid [P.Update GroupName gname P.Assign]
          redirect $ GroupR gid
        Just _ -> do
          let fails :: [Text] = ["There is already group with such name"]
          defaultLayout $(widgetFile "Groups/edit")
    FormFailure fails -> do
      let group = g
      defaultLayout $(widgetFile "Groups/edit")
    _ -> redirect GroupsR

getGroupAttachUserR :: GroupId -> Handler Html
getGroupAttachUserR gid = do
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
  defaultLayout $(widgetFile "Groups/attach_user")

getGroupNewPermissionR :: GroupId -> Handler Html
getGroupNewPermissionR gid = _getGroupNewPermissionR [] gid

_getGroupNewPermissionR :: [Text] -> GroupId -> Handler Html
_getGroupNewPermissionR fails gid = do
  group <- runDB $ P.get404 gid
  (widget, enctype) <- generateFormPost $ newGroupPermission $ Just gid
  defaultLayout $(widgetFile "Groups/new_permission")
