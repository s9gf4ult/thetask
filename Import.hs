module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
-- import           Yesod                as Import hiding (Route (..))
import           Yesod                as Import (runDB)
import           Yesod.Form           as Import
import           Yesod.Core           as Import hiding (Value(..))
-- import qualified Yesod.Persist        as P
-- import qualified Database.Persist     as PP

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text)

import           Foundation           as Import
import           Model                as Import
import           PathPieces           as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
