{-# LANGUAGE
    ScopedTypeVariables
  #-}
module Handler.Users where

import Import
import Forms.User

getUsersR :: CollectionAction -> Handler Html
getUsersR EmptyColAction = do
  users <- runDB $ selectList [] []
  defaultLayout $(widgetFile "Users/index")
getUsersR NewColAction = do
  (widget, enctype) <- generateFormPost $ userForm Nothing
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Users/new")

postUsersR :: CollectionAction -> Handler Html
postUsersR EmptyColAction = do
  ((res, widget), enctype) <- runFormPost (userForm Nothing)
  case res of
    FormSuccess user -> do
      suser <- runDB $ getBy $ UniqueUserEmail $ userEmail user
      case suser of
        Just _ -> do
          let fails :: [Text] = ["There is already one user with such email"]
          defaultLayout $(widgetFile "Users/new")
        Nothing -> do
          uid <- runDB $ insert user
          redirect $ UserR uid EmptyMembAction

    FormFailure fails -> defaultLayout $(widgetFile "Users/new")
postUsersR _ = notFound
