{-# LANGUAGE
  ScopedTypeVariables
  #-}

module Handler.Groups where

import Import
import Forms.Group

getGroupsR :: CollectionAction -> Handler Html
getGroupsR EmptyColAction = do
  groups <- runDB $ selectList [] []
  defaultLayout $(widgetFile "Groups/index")
getGroupsR NewColAction = do
  (widget, enctype) <- generateFormPost $ groupForm Nothing
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Groups/new")

postGroupsR :: CollectionAction -> Handler Html
postGroupsR EmptyColAction = do
  ((res, widget), enctype) <- runFormPost $ groupForm Nothing
  case res of
    FormSuccess group -> do
      oldg <- runDB $ getBy $ UniqueGroup $ groupName group
      case oldg of
        Nothing -> do
          gid <- runDB $ insert group
          redirect $ GroupR gid EmptyMembAction
        Just g -> do
          let fails :: [Text] = ["There is one group with such name"]
          defaultLayout $(widgetFile "Groups/new")
    FormFailure fails -> defaultLayout $(widgetFile "Groups/new")
postGroupsR NewColAction = notFound
