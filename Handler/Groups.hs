module Handler.Groups where

import Import

getGroupsR :: CollectionAction -> Handler Html
getGroupsR EmptyColAction = do
  groups <- runDB $ selectList
  defaultLayout $(widgetFile "Groups/index")
getGroupsR NewColAction = do
  (widget, enctype) <- generateFormPost $ groupForm Nothing

postGroupsR :: CollectionAction -> Handler Html
postGroupsR EmptyMembAction = do
