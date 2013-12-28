module PathPieces where

import Yesod.Core.Dispatch
import Data.Typeable
import Prelude
import Control.Applicative
import Model


data (PathMultiPiece ma, PathMultiPiece ca, PathPiece e) =>
     Piece e ma ca = MPiece e ma -- Member piece
                   | CPiece ca   -- Collection piece
                     deriving (Eq, Show, Read)

instance (PathPiece e, PathMultiPiece ma, PathMultiPiece ca) =>
         PathMultiPiece (Piece e ma ca) where
  fromPathMultiPiece [] = CPiece <$> fromPathMultiPiece []
  fromPathMultiPiece l@(e:rest) = case fromPathPiece e of
    Just eval -> MPiece eval <$> fromPathMultiPiece rest
    Nothing -> CPiece <$> fromPathMultiPiece l

  toPathMultiPiece (MPiece e ma) = (toPathPiece e):(toPathMultiPiece ma)
  toPathMultiPiece (CPiece ca) = toPathMultiPiece ca

data MAction = MEmpty
             | MEdit
             | MDelete
             deriving (Eq, Show, Read)

instance PathMultiPiece MAction where
  fromPathMultiPiece [] = Just MEmpty
  fromPathMultiPiece ["edit"] = Just MEdit
  fromPathMultiPiece ["delete"] = Just MDelete
  fromPathMultiPiece _ = Nothing

  toPathMultiPiece MEmpty = []
  toPathMultiPiece MEdit = ["edit"]
  toPathMultiPiece MDelete = ["delete"]


data CAction = CEmpty
             | CNew
             deriving (Eq, Show, Read)

instance PathMultiPiece CAction where
  fromPathMultiPiece [] = Just CEmpty
  fromPathMultiPiece ["new"] = Just CNew
  fromPathMultiPiece _ = Nothing

  toPathMultiPiece CEmpty = []
  toPathMultiPiece CNew = ["new"]

data MGroupAction = MGroupStd MAction
                  | MGroupNewPermission
                  | MGroupAttachUser
                  deriving (Eq, Show, Read)

instance PathMultiPiece MGroupAction where
  fromPathMultiPiece ["new_permission"] = Just MGroupNewPermission
  fromPathMultiPiece ["attach_user"] = Just MGroupAttachUser
  fromPathMultiPiece x = MGroupStd <$> fromPathMultiPiece x

  toPathMultiPiece (MGroupStd a) = toPathMultiPiece a
  toPathMultiPiece MGroupNewPermission = ["new_permission"]
  toPathMultiPiece MGroupAttachUser = ["attach_user"]

type UserPieces = Piece UserId MAction CAction
