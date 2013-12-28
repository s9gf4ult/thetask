{-# LANGUAGE
  ScopedTypeVariables
  #-}

module Handler.UserGroups where

import Import
import Forms.UserGroup
import qualified Database.Persist as P
import qualified Yesod.Persist    as P

postUserGroupsR :: [Text] -> Handler Html
postUserGroupsR [] = do
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
          redirect $ GroupR gid []
        Just _ -> do
          redirect $ GroupR gid []
    FormFailure fails -> do
      redirect $ GroupsR []

postUserGroupsR _ = notFound
