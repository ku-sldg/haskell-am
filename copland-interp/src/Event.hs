module Event where

import CoplandLang

-- TODO: Do we need to include event ids(unique) in an implementation?  Necessary only for verification phase?
data Event
  = Copy --Ev
-- TODO:  is accumulated Ev implicit in VM context? Same for Sign, Hash, Split
  | Kmeas Pl ASP_ID [ARG] 
  | Umeas ASP_ID [ARG]
  | Sign --Ev
  | Hash --Pl Ev -- TODO: tag with place?
  | Req Pl Pl T --Ev  -- TODO:  do we need to include "from place"? Same for Rpy
  | Rpy Pl Pl -- TODO: include evidence "type" for decoding?
  | Split SP --Ev
  | Join --TODO:  are the two branches to be joined obvious from context?  Need handles?  {- nat → Plc → Evidence → Evidence → Evidence → Ev. -}


