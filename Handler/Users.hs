module Handler.Users where

import PathPieces
import Import

getUsersR :: CollectionAction -> Handler Html
getUsersR EmptyColAction = do
  users <- runDB $ selectList [] []
  defaultLayout $(widgetFile "Users/index")
getUsersR NewColAction = error "not implemented"


postUsersR :: CollectionAction -> Handler Html
postUsersR _ = error "Not yet implemented: postUsersR"
