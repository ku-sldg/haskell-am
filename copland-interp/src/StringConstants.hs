{-# LANGUAGE OverloadedStrings #-}
module StringConstants where

import Data.Text (Text)

{----------- CONSTANT STRINGS -----------}

{--- Concrete Evidence ---}

{- Main concrete evidence datatype name -}
evStr :: String
evStr = "Ev"

{- Concrete Evidence constructor names -}
mtStr :: String
mtStr = "Mt"

uStr :: String
uStr = "U"

kStr :: String
kStr = "K"

gStr :: String
gStr = "G"

hStr :: String
hStr = "H"

nStr :: String
nStr = "N"

ssStr :: String
ssStr = "SS"

ppStr :: String
ppStr = "PP"

{--- Protocol Terms ---}

{- Main protocol term datatype name -}
tStr :: String
tStr = "T"

{- Protocol Term constructor names -}
usmStr :: String
usmStr = "USM"

kimStr :: String
kimStr = "KIM"

sigStr :: String
sigStr = "SIG"

hshStr :: String
hshStr = "HSH"

nonceStr :: String
nonceStr = "NONCE"

atStr :: String
atStr = "AT"

lnStr :: String
lnStr = "LN"

brsStr :: String
brsStr = "BRS"

brpStr :: String
brpStr = "BRP"

{--- JSON tag strings ---}
{- Every JSON object representing an Alegbraic Datatype
   has three members:
   1) typeStr-maps to the datatype name string (e.g. "T" or "Ev").
   2) nameStr-maps to the constructor name string (e.g. "KIM" or "USM").
   3) dataStr-maps to a JSON object that holds the arguments for
        that particular constructor(members of that object will
        differ from constructor to constructor).  -}

{- Datatype -}
typeStr :: Text
typeStr = "type"

{- Constructor -}
nameStr :: Text
nameStr = "name"

{- Constructor arguments -}
dataStr :: Text
dataStr = "data"
