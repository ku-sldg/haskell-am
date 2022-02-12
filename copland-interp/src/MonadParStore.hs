module MonadParStore where

import Copland

import Control.Monad.State
import qualified Data.Map as M



type ParStoreM = StateT ParStore_St IO

data ParStore_St =
  ParStore_St { storeEvMap :: M.Map Loc RawEv }

empty_ParStore_state = ParStore_St {storeEvMap = M.empty}

runParStore :: ParStoreM a -> ParStore_St -> IO (a, ParStore_St)
runParStore k st = runStateT k st
