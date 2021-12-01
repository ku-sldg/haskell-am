{- Custom pretty-printers for Copland language terms.

  Author: Adam Petz
  Date:  06/14/2019
-}

{-# LANGUAGE OverloadedStrings #-}

module DisplayCopland where

import Term_Defs
import Term_Defs_Deriving
import StringConstants

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String (renderShowS, renderString)
import qualified Data.Text as T (pack, Text)
import qualified Data.ByteString as B (take, ByteString, append)

fromStr :: String -> Doc ann
fromStr s = pretty (T.pack s)

instance Pretty ASP where
  pretty t =
    case t of
      (ASPC (Coq_asp_paramsC i args p tid)) ->
        hsep [(fromStr usmStr),
              (viaShow i),
              (viaShow args),
              (viaShow p),
              (viaShow tid)]
      (SIG) -> (fromStr sigStr)
      (HSH) -> (fromStr hshStr)
      (CPY) -> (fromStr "CPY") --TODO: add static cpyStr

instance Pretty Term where
  pretty t =
    case t of
        Coq_asp a -> pretty a
        {-(NONCE) -> (fromStr nonceStr) -}
        (Coq_att p t') -> nest 2 (vsep ["@_" <> (viaShow p), (pretty t')])
        (Coq_lseq t1 t2) ->
          vsep [(fromStr lnStr), (indent 2 (pretty t1)), (indent 2 (pretty t2))]
        (Coq_bseq (sp1,sp2) t1 t2) ->
          vsep [(fromStr (brsStr ++ " (" ++ (show sp1) ++ "," ++ (show sp2) ++ ")")), (indent 2 (pretty t1)), (indent 2 (pretty t2))]
        (Coq_bpar (sp1,sp2) t1 t2) ->
          vsep [(fromStr (brpStr ++ " (" ++ (show sp1) ++ "," ++ (show sp2) ++ ")")), (indent 2 (pretty t1)), (indent 2 (pretty t2))]

  {-
instance Show T where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty
-}


prettyT :: Term -> String
prettyT = renderString . layoutPretty defaultLayoutOptions . pretty

--prettyT s = prettyT' 

shorb :: B.ByteString -> B.ByteString
shorb b =
  let len = 4 in
  B.append (B.take len b) "..."

{-
instance Pretty EvidenceC where
  pretty e =
    case e of
    Mt -> (fromStr mtStr)
    U i args b e' ->
      hsep [(fromStr uStr), {-(viaShow p),-} (viaShow i), (viaShow args),
            (viaShow (shorb b)), parens (pretty e')]
    G p b e' ->
      (hsep [(fromStr gStr), (viaShow p)]) <+> align (vsep [parens (pretty e'), (viaShow (shorb b))])
    H p b e ->
      hsep [(fromStr hStr), (viaShow p), (viaShow (shorb b)), (viaShow e)]
    N n b e' ->
      (fromStr nStr) <+> align (vsep [(viaShow n), (viaShow (shorb b)),
                                      parens (pretty e')])
    SS e1 e2 ->
      vsep [(fromStr ssStr), (indent 2 (pretty e1)), (indent 2 (pretty e2))]

    PP e1 e2 ->
      vsep [(fromStr ppStr), (indent 2 (pretty e1)), (indent 2 (pretty e2))]
{-      
instance Show Ev where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty
-}

prettyEv :: EvidenceC -> String
prettyEv = renderString . layoutPretty defaultLayoutOptions . pretty
-}











{-

df :: IO ()
df = do
  let
    r = (AT 4 (ASPT SIG))
    s = LN (ASPT HSH) (AT 3 r)
    t = LN (AT 4 (ASPT SIG)) (LN (ASPT HSH) (AT 3 (AT 4 (ASPT SIG))))--(LN r s)
    --q = LN (LN (LN NONCE NONCE) NONCE) (LN SIG SIG)
  putStrLn (prettyT t)
-}

{-
as :: IO ()
as = do
  let
    g = G "2sig" Mt
    e = G "3sig" (U 42 [] "abc" g)
    f = G "1sig" e 
    h = SS f g
    k = PP h h
  putStrLn (prettyEv k)
-}
