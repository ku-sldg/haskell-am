module List where

import qualified Prelude

firstn :: Prelude.Int -> (([]) a1) -> ([]) a1
firstn n l =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> ([]))
    (\n0 -> case l of {
             ([]) -> ([]);
             (:) a l0 -> (:) a (firstn n0 l0)})
    n

skipn :: Prelude.Int -> (([]) a1) -> ([]) a1
skipn n l =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> l)
    (\n0 -> case l of {
             ([]) -> ([]);
             (:) _ l0 -> skipn n0 l0})
    n

