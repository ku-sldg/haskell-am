{- Custom pretty-printers for Copland language terms.

  Author: Adam Petz
  Date:  06/14/2019
-}

{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}

module DisplayCopland where

import CoplandLang
import StringConstants

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String (renderShowS, renderString)
import qualified Data.Text as T (pack, Text)
import qualified Data.ByteString as B (take, ByteString, append)

fromStr :: String -> Doc ann
fromStr s = pretty (T.pack s)

instance Pretty T where
  pretty t =
    case t of
        (USM i args) ->
          hsep [(fromStr usmStr), (viaShow i), (viaShow args)]
        (KIM i p args) ->
          hsep [(fromStr kimStr), (viaShow i), (viaShow p), (viaShow args)]
        (SIG) -> (fromStr sigStr)
        (HSH) -> (fromStr hshStr)
        (CPY) -> (fromStr "CPY") --TODO: add static cpyStr
        {-(NONCE) -> (fromStr nonceStr) -}
        (AT p t') -> nest 2 (vsep ["@_" <> (viaShow p), (pretty t')])
        (LN t1 t2) ->
          vsep [(fromStr lnStr), (indent 2 (pretty t1)), (indent 2 (pretty t2))]
        (BRS (sp1,sp2) t1 t2) ->
          vsep [(fromStr (brsStr ++ " (" ++ (show sp1) ++ "," ++ (show sp2) ++ ")")), (indent 2 (pretty t1)), (indent 2 (pretty t2))]
        (BRP (sp1,sp2) t1 t2) ->
          vsep [(fromStr (brpStr ++ " (" ++ (show sp1) ++ "," ++ (show sp2) ++ ")")), (indent 2 (pretty t1)), (indent 2 (pretty t2))]

  {-
instance Show T where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty
-}

prettyT = renderString . layoutPretty defaultLayoutOptions . pretty

--prettyT s = prettyT' 

shorb :: B.ByteString -> B.ByteString
shorb b =
  let len = 4 in
  B.append (B.take len b) "..."

instance Pretty Ev where
  pretty e =
    case e of
    Mt -> (fromStr mtStr)
    U i args p b e' ->
      hsep [(fromStr uStr), (viaShow i), (viaShow args), (viaShow p),
            (viaShow (shorb b)), parens (pretty e')]
    K i args p q b e' ->
      hsep [(fromStr kStr), (viaShow i), (viaShow args), (viaShow p),
            (viaShow q),(viaShow (shorb b)), parens (pretty e')]
    G p e' b ->
      (fromStr gStr) <+> align (vsep [(viaShow p), parens (pretty e'), (viaShow (shorb b))])
    H p b ->
      hsep [(fromStr hStr), (viaShow p), (viaShow (shorb b))]
    N p n b e' ->
      (fromStr nStr) <+> align (vsep [(viaShow p), (viaShow n), (viaShow (shorb b)),
                                      parens (pretty e')])
    SS e1 e2 ->
      vsep [(fromStr ssStr), (indent 2 (pretty e1)), (indent 2 (pretty e2))]

    PP e1 e2 ->
      vsep [(fromStr ppStr), (indent 2 (pretty e1)), (indent 2 (pretty e2))]
{-      
instance Show Ev where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty
-}

prettyEv = renderString . layoutPretty defaultLayoutOptions . pretty













df :: IO ()
df = do
  let
    r = (AT 4 SIG)
    s = LN HSH (AT 3 r)
    t = LN (AT 4 SIG) (LN HSH (AT 3 (AT 4 SIG)))--(LN r s)
    --q = LN (LN (LN NONCE NONCE) NONCE) (LN SIG SIG)
  putStrLn (prettyT t)

as :: IO ()
as = do
  let
    g = G 2 Mt "2sig"
    e = G 3 (U 42 [] 1 "abc" g) "3sig"
    f = G 1 e "1sig"
    h = SS f g
    k = PP h h
  putStrLn (prettyEv k)
