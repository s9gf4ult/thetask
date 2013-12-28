module Forms.GroupPermission where


import Import
import Fields

gpvalues :: [(Text, PermissionValues)]
gpvalues = [("Admin rights", PVAdmin),
            ("Delegate admin rights", PVDelegateAdmin)]


newGroupPermission :: Maybe GroupId -> Form GroupPermission
newGroupPermission gid = renderDivs
                     $ GroupPermission
                     <$> areq hiddenField "" gid
                     <*> areq (selectFieldList gpvalues) "choose permission" Nothing
