{-# LANGUAGE
  ScopedTypeVariables
  #-}

module Handler.UserGroups where

import Import
import Forms.UserGroup

postUserGroupsR :: [Text] -> Handler Html
postUserGroupsR [] = do
  ((res, _), _) <- runFormPost $ userGroupCreateForm Nothing Nothing
  case res of
    FormSuccess usergroup -> do
      let uid = userGroupUserId usergroup
          gid = userGroupGroupId usergroup
      user <- runDB $ get404 uid
      group <- runDB $ get404 gid
      oldug <- runDB $ getBy $ UniqueUserGroup uid gid
      case oldug of
        Nothing -> do
          runDB $ insert usergroup
          redirect $ GroupR gid []
        Just _ -> do
          redirect $ GroupR gid []
    FormFailure fails -> do
      redirect $ GroupsR []

postUserGroupsR _ = notFound
