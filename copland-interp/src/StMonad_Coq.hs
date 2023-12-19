{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module StMonad_Coq where

import qualified Prelude
import qualified Control.Monad.State.Lazy as SL
import MonadCop (COP)

type St s = SL.StateT s COP

ret :: a -> St s a
ret = Prelude.return

bind m f = m Prelude.>>= f

modify :: (a1 -> a1) -> St a1 ()
modify = SL.modify

put :: s -> St s ()
put = SL.put

get :: St s s
get = SL.get

execSt :: (St a1 a2) -> a1 -> COP a1
execSt m st = SL.execStateT m st 
