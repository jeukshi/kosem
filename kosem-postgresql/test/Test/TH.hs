{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.TH where

import Data.Text qualified as T
import Language.Haskell.TH (Exp, Q, appE, stringE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

text :: QuasiQuoter
text =
    QuasiQuoter
        { quotePat = error "quasiquoter used in pattern context"
        , quoteType = error "quasiquoter used in type context"
        , quoteDec = error "quasiquoter used in declaration context"
        , quoteExp = sqlExp
        }

sqlExp :: String -> Q Exp
sqlExp = appE [|T.pack|] . stringE
