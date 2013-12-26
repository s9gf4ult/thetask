{-# LANGUAGE
  ScopedTypeVariables
  #-}
module Handler.Group where

import Import
import Forms.Group
import qualified Database.Esqueleto as E

getGroupR :: GroupId -> MemberAction -> Handler Html
getGroupR gid EmptyMembAction = do
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
getGroupR gid EditMembAction = do
  group <- runDB $ get404 gid
  (widget, enctype) <- generateFormPost $ groupForm $ Just group
  let fails :: [Text] = []
  defaultLayout $(widgetFile "Group/edit")

postGroupR :: GroupId -> MemberAction -> Handler Html
postGroupR gid EmptyMembAction = do
  g <- runDB $ get404 gid
  ((res, widget), enctype) <- runFormPost $ groupForm Nothing
  case res of
    FormSuccess group -> do
      let gname = groupName group
      oldg <- runDB $ getBy $ UniqueGroup gname
      case oldg of
        Nothing -> do
          runDB $ update gid [Update GroupName gname Assign]
          redirect $ GroupR gid EmptyMembAction
        Just _ -> do
          let fails :: [Text] = ["There is already group with such name"]
          defaultLayout $(widgetFile "Group/edit")
    FormFailure fails -> do
      let group = g
      defaultLayout $(widgetFile "Group/edit")


postGroupR _ _ = notFound

deleteGroupR :: GroupId -> MemberAction -> Handler Html
deleteGroupR = error "Not yet implemented: deleteGroupR"
