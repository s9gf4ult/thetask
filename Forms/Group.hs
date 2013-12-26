module Forms.Group where

import Import


groupForm :: Maybe Group -> Form Group
groupForm mg = renderDivs
               $ Group
               <$> areq textField "group name" (groupName <$> mg)
