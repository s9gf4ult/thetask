{-# LANGUAGE
  ScopedTypeVariables
  #-}
module Handler.User where

import Import
import Forms.User

getUserR :: UserId -> MemberAction -> Handler Html
getUserR uid EmptyMembAction = do
  user <- runDB $ get404 uid
  defaultLayout $(widgetFile "User/show")

getUserR uid EditMembAction = do
  user <- runDB $ get404 uid
  let fails :: [Text] = []
  (widget, enctype) <- generateFormPost $ userForm $ Just user
  defaultLayout $(widgetFile "User/edit")

deleteUserR :: UserId -> MemberAction -> Handler Html
deleteUserR uid EmptyMembAction = error "not implemented"
deleteUserR _ _ = notFound

postUserR :: UserId -> MemberAction ->  Handler Html
postUserR uid EmptyMembAction = do
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
          redirect $ UserR uid EmptyMembAction
    FormFailure fails -> do
      user <- runDB $ get404 uid
      defaultLayout $(widgetFile "User/edit")
postUserR _ _ = notFound
