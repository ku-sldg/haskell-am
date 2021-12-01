module Datatypes where

import qualified Prelude

snd :: ((,) a1 a2) -> a2
snd p =
  case p of {
   (,) _ y -> y}

app :: (([]) a1) -> (([]) a1) -> ([]) a1
app l m =
  case l of {
   ([]) -> m;
   (:) a l1 -> (:) a (app l1 m)}

