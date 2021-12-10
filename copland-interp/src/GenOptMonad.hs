{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module GenOptMonad where

import qualified Prelude
import qualified Control.Monad.State.Lazy as SL
--import MonadCop (COP)
import qualified MonadAM (AM)

--type AM a = SL.StateT s COP
type AM = MonadAM.AM

type Opt = MonadAM.AM

ret :: a -> AM a
ret = SL.return

bind m f = m SL.>>= f

failm = Prelude.error "ERROR in AM Monad computation"

{-
modify :: (a1 -> a1) -> St a1 ()
modify = SL.modify

put :: s -> St s ()
put = SL.put

get :: St s s
get = SL.get

execSt :: (St a1 a2) -> a1 -> COP a1
execSt m st = SL.execStateT m st
-}
