{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module GenStMonad where

import qualified Prelude
import qualified Control.Monad.State.Lazy as SL

type St s = SL.StateT s (Prelude.IO)

ret :: a -> St s a
ret = SL.return

bind m f = m SL.>>= f

modify :: (a1 -> a1) -> St a1 ()
modify = SL.modify

put :: s -> St s ()
put = SL.put

get :: St s s
get = SL.get

execSt :: (St a1 a2) -> a1 -> Prelude.IO a1
execSt = SL.execStateT
