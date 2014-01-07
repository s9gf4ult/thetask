{-# LANGUAGE
  ScopedTypeVariables
  #-}

module Handler.GroupPermissions where

import Control.Monad (when)
import Fields
import Forms.GroupPermission
import Handler.Groups  (_getGroupNewPermissionR)
import Import
import Models.User
import Yesod.Auth (maybeAuthId)
import qualified Data.Set as S
import qualified Database.Persist     as P
import qualified Yesod.Persist        as P

postGroupPermissionsNewR :: Handler Html
postGroupPermissionsNewR = do
  ((res, _), _) <- runFormPost $ newGroupPermission Nothing
  case res of
    FormSuccess perm -> do
      let gid = groupPermissionGroupId perm
          pval = groupPermissionValue perm
      when (pval == PVAdmin) $ do
        aid <- maybeAuthId
        case aid of
          Nothing -> notAuthenticated
          Just uid -> do
            p <- userPermissions uid
            when (not $ S.isSubsetOf (S.fromList [PVDelegateAdmin, PVAdmin]) p)
              $ permissionDenied "You must have admin delegate rights"

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
