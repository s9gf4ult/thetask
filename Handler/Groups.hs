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



getGroupsR :: GroupPieces -> Handler Html
getGroupsR (CPiece CEmpty) = do
  groups <- runDB $ P.selectList [] []
  defaultLayout $(widgetFile "Groups/index")
getGroupsR (CPiece CNew) = do
  (widget, enctype) <- generateFormPost $ groupForm Nothing
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Groups/new")

getGroupsR (MPiece gid (MGroupStd MEmpty)) = do
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
getGroupsR (MPiece gid (MGroupStd MEdit)) = do
  group <- runDB $ P.get404 gid
  (widget, enctype) <- generateFormPost $ groupForm $ Just group
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Group/edit")
getGroupsR (MPiece gid MGroupAttachUser) = do
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
getGroupsR (MPiece gid MGroupNewPermission) = do
  group <- runDB $ P.get404 gid
  (widget, enctype) <- generateFormPost $ newGroupPermission $ Just gid
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Group/new_permission")
getGroupsR _ = notFound


postGroupsR :: GroupPieces -> Handler Html
postGroupsR (CPiece CEmpty) = do
  ((res, widget), enctype) <- runFormPost $ groupForm Nothing
  case res of
    FormSuccess group -> do
      oldg <- runDB $ P.getBy $ UniqueGroup $ groupName group
      case oldg of
        Nothing -> do
          gid <- runDB $ P.insert group
          redirect $ GroupsR $ MPiece gid $ MGroupStd MEmpty
        Just g -> do
          let fails :: [Text] = ["There is one group with such name"]
          defaultLayout $(widgetFile "Groups/new")
    FormFailure fails -> defaultLayout $(widgetFile "Groups/new")
postGroupsR (MPiece gid (MGroupStd MEmpty)) = do
  g <- runDB $ P.get404 gid
  ((res, widget), enctype) <- runFormPost $ groupForm Nothing
  case res of
    FormSuccess group -> do
      let gname = groupName group
      oldg <- runDB $ P.getBy $ UniqueGroup gname
      case oldg of
        Nothing -> do
          runDB $ P.update gid [P.Update GroupName gname P.Assign]
          redirect $ GroupsR $ MPiece gid $ MGroupStd MEmpty
        Just _ -> do
          let fails :: [Text] = ["There is already group with such name"]
          defaultLayout $(widgetFile "Group/edit")
    FormFailure fails -> do
      let group = g
      defaultLayout $(widgetFile "Group/edit")
postGroupsR _ = notFound
