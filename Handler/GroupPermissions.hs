{-# LANGUAGE
  ScopedTypeVariables
  #-}

module Handler.GroupPermissions where

import Forms.GroupPermission
import Handler.Groups  (_getGroupNewPermissionR)
import Import
import qualified Database.Persist     as P
import qualified Yesod.Persist        as P

postGroupPermissionsNewR :: Handler Html
postGroupPermissionsNewR = do
  ((res, widget), enctype) <- runFormPost $ newGroupPermission Nothing
  case res of
    FormSuccess perm -> do
      let gid = groupPermissionGroupId perm
          pval = groupPermissionValue perm
      oldperm <- runDB $ P.getBy $ UniqueGroupPermission gid pval
      case oldperm of
        Nothing -> do
          runDB $ P.insert_ $ GroupPermission gid pval
          redirect $ GroupR gid
        Just _ -> do
          _getGroupNewPermissionR ["This permission already granted"] gid
    _ -> redirect $ GroupsR

postGroupPermissionDeleteR :: GroupPermissionId -> Handler Html
postGroupPermissionDeleteR gpid = do
  gperm <- runDB $ P.get404 gpid
  let gid = groupPermissionGroupId gperm
  runDB $ P.delete gpid
  redirect $ GroupR gid
