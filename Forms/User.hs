module Forms.User where

import Import
import qualified Database.Persist     as P


userForm :: Maybe User -> Form (Text, Text)
userForm mu h = do
  (res, widget) <- renderDivs ((,,)
                               <$> areq emailField "Email" (userEmail <$> mu)
                               <*> areq passwordField "Password" Nothing
                               <*> areq passwordField "Confirm password" Nothing) h
  return (checkPass res, widget)
  where
    checkPass (FormSuccess (email, p1, p2)) | p1 == p2 = FormSuccess (email, p1)
                                            | otherwise = FormFailure ["You must input password correctly twice"]
    checkPass (FormFailure f) = FormFailure f
    checkPass FormMissing = FormMissing


editUserForm :: User -> Form [P.Update User]
editUserForm user = renderDivs $ upd <$> aopt emailField "Change email" (Just $ userEmail user)
  where
    upd (Just email) = [P.Update UserEmail email P.Assign]
    upd Nothing = []

changePasswordForm :: Form (Text, Text) -- ^ new password, old password
changePasswordForm h = do
  (res, widget) <- renderDivs ((,,)
                               <$> areq passwordField "New password" Nothing
                               <*> areq passwordField "New password again" Nothing
                               <*> areq passwordField "Old password" Nothing) h
  return (checkPass res, widget)
  where
    checkPass (FormSuccess (p1, p2, oldp)) | p1 == p2 = if oldp /= p1
                                                        then FormSuccess (p1, oldp)
                                                        else FormFailure ["Old password is just same as new"]
                                           | otherwise = FormFailure ["You must input password correctly twice"]
    checkPass FormFailure f = FormFailure f
    checkPass FormMissing = FormMissing
