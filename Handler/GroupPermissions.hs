{-# LANGUAGE
  ScopedTypeVariables
  #-}

module Handler.GroupPermissions where

import Import
import Forms.GroupPermission
import qualified Database.Persist     as P
import qualified Yesod.Persist        as P


postGroupPermissionsR :: GroupPermissionPieces -> Handler Html
postGroupPermissionsR (CPiece CEmpty) = do
  ((res, widget), enctype) <- runFormPost $ newGroupPermission Nothing
  case res of
    FormSuccess perm -> do
      let gid = groupPermissionGroupId perm
          pval = groupPermissionValue perm
      oldperm <- runDB $ P.getBy $ UniqueGroupPermission gid pval
      case oldperm of
        Nothing -> do
          _ <- runDB $ P.insert $ GroupPermission gid pval
          redirect $ GroupsR $ MPiece gid $ MGroupStd MEmpty
        Just _ -> do
          let fails :: [Text] = ["This permission already granted"]
          group <- runDB $ P.get404 gid
          defaultLayout $(widgetFile "Groups/new_permission")
    _ -> redirect $ GroupsR $ CPiece CEmpty
postGroupPermissionsR (MPiece gpid MDelete) = do
  gperm <- runDB $ P.get404 gpid
  let gid = groupPermissionGroupId gperm
  runDB $ P.delete gpid
  redirect $ GroupsR $ MPiece gid $ MGroupStd MEmpty
postGroupPermissionsR _ = notFound
