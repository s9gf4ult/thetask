{-# LANGUAGE
    ScopedTypeVariables
  #-}
module Handler.Users where

import Import
import Forms.User

getUsersR :: [Text] -> Handler Html
getUsersR [] = do
  users <- runDB $ selectList [] []
  defaultLayout $(widgetFile "Users/index")
getUsersR ["new"] = do
  (widget, enctype) <- generateFormPost $ userForm Nothing
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Users/new")
getUsersR x = notFound

postUsersR :: [Text] -> Handler Html
postUsersR [] = do
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
          redirect $ UserR uid []

    FormFailure fails -> defaultLayout $(widgetFile "Users/new")
postUsersR _ = notFound
