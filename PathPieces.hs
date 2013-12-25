module PathPieces where

import Yesod.Core.Dispatch
import Data.Typeable
import Prelude

data CollectionAction = EmptyColAction
                      | NewColAction
                        deriving (Typeable, Show, Eq, Read)

instance PathMultiPiece CollectionAction where
  fromPathMultiPiece [] = Just EmptyColAction
  fromPathMultiPiece ["new"] = Just NewColAction
  fromPathMultiPiece _ = Nothing

  toPathMultiPiece EmptyColAction = []
  toPathMultiPiece NewColAction = ["new"]
