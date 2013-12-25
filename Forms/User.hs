module Forms.User where

import Import


userForm :: Maybe User -> Form User
userForm mu = renderDivs
              $ User
              <$> areq textField "user email" (userEmail <$> mu)
              <*> areq textField "user password" (userPassword <$> mu)
