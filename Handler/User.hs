{-# LANGUAGE
  ScopedTypeVariables
  #-}
module Handler.User where

import Import
import Forms.User

getUserR :: UserId -> [Text] -> Handler Html
getUserR uid [] = do
  user <- runDB $ get404 uid
  defaultLayout $(widgetFile "User/show")

getUserR uid ["edit"] = do
  user <- runDB $ get404 uid
  let fails :: [Text] = []
  (widget, enctype) <- generateFormPost $ userForm $ Just user
  defaultLayout $(widgetFile "User/edit")

deleteUserR :: UserId -> [Text] -> Handler Html
deleteUserR uid [] = error "not implemented"
deleteUserR _ _ = notFound

postUserR :: UserId -> [Text] ->  Handler Html
postUserR uid [] = do
  ((res, widget), enctype) <- runFormPost $ userForm Nothing
  case res of
    FormSuccess user -> do
      suser <- runDB $ getBy $ UniqueUserEmail $ userEmail user
      case suser of
        Just _ -> do
          let fails :: [Text] = ["There is already user with such email"]
          defaultLayout $(widgetFile "User/edit")
        Nothing -> do
          runDB $ update uid [Update UserEmail (userEmail user) Assign]
          redirect $ UserR uid [""]
    FormFailure fails -> do
      user <- runDB $ get404 uid
      defaultLayout $(widgetFile "User/edit")
postUserR _ _ = notFound
