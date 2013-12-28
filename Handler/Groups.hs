{-# LANGUAGE
  ScopedTypeVariables
  #-}

module Handler.Groups where

import Import
import Forms.Group
import qualified Database.Persist     as P
import qualified Yesod.Persist        as P


getGroupsR :: [Text] -> Handler Html
getGroupsR [] = do
  groups <- runDB $ P.selectList [] []
  defaultLayout $(widgetFile "Groups/index")
getGroupsR ["new"] = do
  (widget, enctype) <- generateFormPost $ groupForm Nothing
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Groups/new")

postGroupsR :: [Text] -> Handler Html
postGroupsR [] = do
  ((res, widget), enctype) <- runFormPost $ groupForm Nothing
  case res of
    FormSuccess group -> do
      oldg <- runDB $ P.getBy $ UniqueGroup $ groupName group
      case oldg of
        Nothing -> do
          gid <- runDB $ P.insert group
          redirect $ GroupR gid []
        Just g -> do
          let fails :: [Text] = ["There is one group with such name"]
          defaultLayout $(widgetFile "Groups/new")
    FormFailure fails -> defaultLayout $(widgetFile "Groups/new")
postGroupsR ["new"] = notFound
