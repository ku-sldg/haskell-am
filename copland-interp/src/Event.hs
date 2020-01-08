module Event where

import CoplandLang
import MonadCop
--import Interp
import Comm


import Control.Monad.Trans.Reader(asks)
import Control.Monad.Trans(liftIO)
import Control.Parallel (pseq)

-- TODO: Do we need to include event ids(unique) in an implementation?  Necessary only for verification phase?
data Event
  = Copy --Ev
-- TODO:  is accumulated Ev implicit in VM context? Same for Sign, Hash, Split
  | Kmeas ASP_ID Pl [ARG] 
  | Umeas ASP_ID [ARG]
  | Sign --Ev
  | Hash --Pl Ev -- TODO: tag with place?
  | ReqRpy Pl Pl T --Ev  -- TODO:  do we need to include "from place"?
-- | Rpy Pl Pl -- TODO: include evidence "type" for decoding?  Or put in VM env?
  | Split SP SP --T Ev
  | BESL Event
  | BESR Event
  | BEP [Event] [Event] -- TODO:  need some primitive component in VM for parallelism
  | JoinS
  | JoinP --TODO:  are the two branches to be joined obvious from context?  Need handles? Need to know parallel vs sequential branch?  {- nat → Plc → Evidence → Evidence → Evidence → Ev. -}
    deriving (Show)

asp_event :: T -> Pl -> Event
asp_event t p =
  case t of
  CPY -> Copy
  KIM i p args -> Kmeas i p args
  USM i args -> Umeas i args
  SIG -> Sign
  HSH -> Hash

event_compiler :: T -> Pl -> [Event]
event_compiler t p =
  case t of
  CPY -> [Copy]
  KIM i p args -> [Kmeas i p args]
  USM i args -> [Umeas i args]
  SIG -> [Sign]
  HSH -> [Hash]
  AT q t' -> [ReqRpy p q t']
    {-let req = Req p q t
        rpy = Rpy p q
        {-tr = event_compiler t q-} in
    [req] ++ {-tr ++-} [rpy] -}
  LN t1 t2 ->
    let tr1 = event_compiler t1 p
        tr2 = event_compiler t2 p in
    tr1 ++ tr2
  BRS (sp1,sp2) t1 t2 ->
    let split = [Split sp1 sp2]
        tr1 = event_compiler t1 p
        tr2 = event_compiler t2 p
        evalL = map BESL tr1
        evalR = map BESR tr2 in
     split ++ evalL ++ evalR ++ [JoinS]
     
  BRP (sp1,sp2) t1 t2 ->
    let split = [Split sp1 sp2]
        tr1 = event_compiler t1 p
        tr2 = event_compiler t2 p
        tr = [BEP tr1 tr2] in
     split ++ tr ++ [JoinP]

{-
justSend :: Pl -> T -> Ev -> COP ()
justSend pTo t e = do
  logc "inside justSend"
  pMe <- asks me
  if (pTo == pMe)
   then do
    error "sent request to MYSELF!" -- TODO:  typehecker should catch this
   else do
    mSock <- do
      pString <- getTheirSock pTo
      sock' <- liftIO $ Comm.client_resolve_open_localhost pString
      return (sock')

    sendReq mSock pTo t e -}
  

