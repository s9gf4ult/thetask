{-# LANGUAGE
    ScopedTypeVariables
  #-}
module Handler.Users where

import Database.Esqueleto
import Forms.User
import Import
import qualified Database.Persist     as P
import qualified Yesod.Persist        as P

getUsersR :: UserPieces -> Handler Html
getUsersR (CPiece CEmpty) = do
  users <- runDB $ P.selectList [] []
  defaultLayout $(widgetFile "Users/index")
getUsersR (CPiece CNew) = do
  (widget, enctype) <- generateFormPost $ userForm Nothing
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Users/new")

getUsersR (MPiece uid MEmpty) = do
  user <- runDB $ P.get404 uid
  groups <- runDB $ select $ from $ \(group `InnerJoin` userGroup) -> do
    on (userGroup ^. UserGroupGroupId ==. group ^. GroupId)
    where_ (userGroup ^. UserGroupUserId ==. (val uid))
    return (userGroup, group)
  defaultLayout $(widgetFile "Users/show")

getUsersR (MPiece uid MEdit) = do
  user <- runDB $ P.get404 uid
  let fails :: [Text] = []
  (widget, enctype) <- generateFormPost $ userForm $ Just user
  defaultLayout $(widgetFile "Users/edit")

getUsersR _ = notFound


postUsersR :: UserPieces -> Handler Html
postUsersR (CPiece CEmpty) = do
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
          redirect $ UsersR $ MPiece uid MEmpty

    FormFailure fails -> defaultLayout $(widgetFile "Users/new")

postUsersR (MPiece uid MEmpty) = do
  ((res, widget), enctype) <- runFormPost $ userForm Nothing
  case res of
    FormSuccess user -> do
      suser <- runDB $ P.getBy $ UniqueUserEmail $ userEmail user
      case suser of
        Just _ -> do
          let fails :: [Text] = ["There is already user with such email"]
          defaultLayout $(widgetFile "Users/edit")
        Nothing -> do
          runDB $ P.update uid [P.Update UserEmail (userEmail user) P.Assign]
          redirect $ UsersR $ MPiece uid MEmpty
    FormFailure fails -> do
      user <- runDB $ P.get404 uid
      defaultLayout $(widgetFile "Users/edit")
postUsersR _ = notFound
