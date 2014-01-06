{-# LANGUAGE
  ScopedTypeVariables
  #-}

module Handler.UserGroups where

import Import
import Forms.UserGroup
import qualified Database.Persist as P
import qualified Yesod.Persist    as P

postUserGroupsR :: UserGroupPieces -> Handler Html
postUserGroupsR (CPiece CEmpty) = do
  ((res, _), _) <- runFormPost $ userGroupCreateForm Nothing Nothing
  case res of
    FormSuccess usergroup -> do
      let uid = userGroupUserId usergroup
          gid = userGroupGroupId usergroup
      user <- runDB $ P.get404 uid
      group <- runDB $ P.get404 gid
      oldug <- runDB $ P.getBy $ UniqueUserGroup uid gid
      case oldug of
        Nothing -> do
          runDB $ P.insert usergroup
          redirect $ GroupR gid
        Just _ -> do
          redirect $ GroupR gid
    _ -> do
      redirect $ GroupsR

postUserGroupsR (MPiece ugid MDelete) = do
  userGroup <- runDB $ P.get404 ugid
  let gid = userGroupGroupId userGroup
  runDB $ P.delete ugid
  redirect $ GroupR gid
postUserGroupsR _ = notFound
