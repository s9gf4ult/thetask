{-# LANGUAGE
    ScopedTypeVariables
  #-}
module Handler.Users where

import Yesod.Auth.HashDB (setPassword)
import Database.Esqueleto
import Forms.User
import Import
import qualified Database.Persist     as P
import qualified Yesod.Persist        as P

getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ P.selectList [] []
  defaultLayout $(widgetFile "Users/index")

getUsersNewR :: Handler Html
getUsersNewR = do
  (widget, enctype) <- generateFormPost $ userForm Nothing
  let fails = [] :: [Text]
  defaultLayout $(widgetFile "Users/new")

postUsersNewR :: Handler Html
postUsersNewR = do
  ((res, widget), enctype) <- runFormPost (userForm Nothing)
  case res of
    FormSuccess (email, pass) -> do
      user <- setPassword pass $ User email "" ""
      suser <- runDB $ insertBy user
      case suser of
        Left _ -> do
          let fails :: [Text] = ["There is already one user with such email"]
          defaultLayout $(widgetFile "Users/new")
        Right uid -> redirect $ UserR uid

    FormFailure fails -> defaultLayout $(widgetFile "Users/new")
    FormMissing -> redirect UsersR

getUserR :: UserId -> Handler Html
getUserR uid = do
  user <- runDB $ P.get404 uid
  groups <- runDB $ select $ from $ \(group `InnerJoin` userGroup) -> do
    on (userGroup ^. UserGroupGroupId ==. group ^. GroupId)
    where_ (userGroup ^. UserGroupUserId ==. (val uid))
    return (userGroup, group)
  defaultLayout $(widgetFile "Users/show")

getUserEditR :: UserId -> Handler Html
getUserEditR uid = _getUserEditR [] uid

_getUserEditR :: [Text] -> UserId -> Handler Html
_getUserEditR fails uid = do
  user <- runDB $ P.get404 uid
  (ewidget, eenctype) <- generateFormPost $ editUserForm user
  (pwidget, penctype) <- generateFormPost $ changePasswordForm
  defaultLayout $(widgetFile "Users/edit")


postUserEditR :: UserId -> Handler Html
postUserEditR uid = error "not implementated"

postUserChpasswdR :: UserId -> Handler Html
postUserChpasswdR uid = do
  ((res, widget), enctype) <- runFormPost changePasswordForm
  case res of
    FormSuccess (newp, oldp) -> do
      user <- runDB $ P.get404 uid
      -- we are admin and so, we do not check old password
      nuser <- setPassword newp user
      runDB $ P.replace uid nuser
      redirect $ UserR uid
    FormFailure fails -> do
      _getUserEditR fails uid
    _ -> redirect $ UserR uid
