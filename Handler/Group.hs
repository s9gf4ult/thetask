{-# LANGUAGE
  ScopedTypeVariables
  #-}
module Handler.Group where

import Import
import Forms.Group
import Forms.UserGroup
import Control.Monad (forM)
import qualified Database.Esqueleto as E

getGroupR :: GroupId -> [Text] -> Handler Html
getGroupR gid [] = do
  group <- runDB $ get404 gid
  users <- runDB
           $ E.select
           $ E.from $ \(user `E.InnerJoin` userGroup `E.InnerJoin` group) -> do
             E.on (user E.^. UserId E.==. userGroup E.^. UserGroupUserId)
             E.on (userGroup E.^. UserGroupGroupId E.==. group E.^. GroupId)
             E.where_ (group E.^. GroupId E.==. (E.val gid))
             E.orderBy [E.asc (user E.^. UserEmail)]
             return (user, userGroup)

  defaultLayout $(widgetFile "Group/show")
getGroupR gid ["edit"] = do
  group <- runDB $ get404 gid
  (widget, enctype) <- generateFormPost $ groupForm $ Just group
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Group/edit")
getGroupR gid ["attach_user"] = do
  group <- runDB $ get404 gid
  users <- runDB
           $ E.select
           $ E.from $ \user -> do
             E.where_ $ E.notExists $ do
               E.from $ \userGroup -> do
                 E.where_ $ (userGroup E.^. UserGroupGroupId E.==. (E.val gid)) E.&&. (userGroup E.^. UserGroupUserId E.==. user E.^. UserId)
               return ()
             return user
  widgets <- forM users $ \(Entity uid user) -> do
    (widget, enctype) <- generateFormPost $ userGroupCreateForm (Just uid) (Just gid)
    return (widget, enctype, user)
  defaultLayout $(widgetFile "Group/attach_user")
getGroupR _ _ = notFound



postGroupR :: GroupId -> [Text] -> Handler Html
postGroupR gid [] = do
  g <- runDB $ get404 gid
  ((res, widget), enctype) <- runFormPost $ groupForm Nothing
  case res of
    FormSuccess group -> do
      let gname = groupName group
      oldg <- runDB $ getBy $ UniqueGroup gname
      case oldg of
        Nothing -> do
          runDB $ update gid [Update GroupName gname Assign]
          redirect $ GroupR gid []
        Just _ -> do
          let fails :: [Text] = ["There is already group with such name"]
          defaultLayout $(widgetFile "Group/edit")
    FormFailure fails -> do
      let group = g
      defaultLayout $(widgetFile "Group/edit")
postGroupR _ _ = notFound

deleteGroupR :: GroupId -> [Text] -> Handler Html
deleteGroupR = error "Not yet implemented: deleteGroupR"
