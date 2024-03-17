module Database.Kosem.PostgreSQL.Internal.Diagnostics.GHC where

import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Parser.Errors.Types (PsMessage (..))
import GHC.Tc.Errors.Types (TcRnMessage (TcRnUnknownMessage))
import GHC.Tc.Utils.Monad (TcM, addErrAt)
import GHC.Types.Error (UnknownDiagnostic (UnknownDiagnostic), mkPlainError, noHints)
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Utils.Outputable (text)
import Language.Haskell.TH.Syntax (Q (Q))
import Unsafe.Coerce (unsafeCoerce)

errorWithSpan :: SrcSpan -> String -> Q ()
errorWithSpan loc msg = unsafeRunTcM $ addErrAt loc ghcMsg
  where
    ghcMsg =
      -- TODO this needs CPP to work with other GHC versions
      -- like: https://github.com/guibou/PyF/blob/main/src/PyF/Internal/QQ.hs#L258
        TcRnUnknownMessage
            . UnknownDiagnostic
            . mkPlainError noHints
            . text
            $ msg

    -- \| Stolen from: https://github.com/guibou/PyF
    -- Which comes from:
    -- https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/
    -- Like PyF, we keep it for errors only, so it "should be fine".
    -- TODO check if it is fine with warnings too, when we have them.
    unsafeRunTcM :: TcM a -> Q a
    unsafeRunTcM m = Q (unsafeCoerce m)
