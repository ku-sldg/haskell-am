module BS where

import qualified Data.ByteString as B (ByteString, empty, singleton)

type BS = B.ByteString

default_bs = B.empty
empty_bs = B.empty

zero_bs = B.singleton 0
one_bs = B.singleton 1

bool_to_bs :: Bool -> BS
bool_to_bs b =
  case b of
    True -> one_bs
    False -> zero_bs
