{-# LANGUAGE
  ScopedTypeVariables
  #-}

module Handler.GroupPermissions where

import Import
import Forms.GroupPermission
import qualified Database.Persist     as P
import qualified Yesod.Persist        as P


postGroupPermissionsR :: [Text] -> Handler Html
postGroupPermissionsR [] = do
  ((res, widget), enctype) <- runFormPost $ newGroupPermission Nothing
  case res of
    FormSuccess perm -> do
      let gid = groupPermissionGroupId perm
          pval = groupPermissionValue perm
      oldperm <- runDB $ P.getBy $ UniqueGroupPermission gid pval
      case oldperm of
        Nothing -> do
          runDB $ P.insert $ GroupPermission gid pval
          redirect $ GroupR gid []
        Just _ -> do
          let fails :: [Text] = ["This permission already granted"]
          group <- runDB $ P.get404 gid
          defaultLayout $(widgetFile "Group/new_permission")
    _ -> redirect $ GroupsR []
postGroupPermissionsR _ = notFound
