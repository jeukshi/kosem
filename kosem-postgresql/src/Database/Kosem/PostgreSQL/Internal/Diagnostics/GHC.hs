{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal.Diagnostics.GHC where

import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString (FastString)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Parser.Errors.Types (PsMessage (..))
import GHC.Tc.Errors.Types (TcRnMessage (TcRnUnknownMessage))
import GHC.Tc.Utils.Monad (TcM, addErrAt)
import GHC.Types.Error (UnknownDiagnostic (UnknownDiagnostic), mkPlainError, noHints)
import GHC.Types.SrcLoc (SrcSpan, mkSrcLoc, mkSrcSpan)
import GHC.Utils.Outputable (text)
import Language.Haskell.TH.Syntax (Q (Q))
import Unsafe.Coerce (unsafeCoerce)

data SourcePoint = SourcePoint
    { filename :: String
    , line :: Int
    , column :: Int
    }

data DiagnosticSpan a
    = DiagnosticSpan a a
    deriving (Show, Eq)

errorWithSpan :: DiagnosticSpan SourcePoint -> Text -> Q ()
errorWithSpan span msg = unsafeRunTcM $ addErrAt (ghcSpan span) ghcMsg
  where
    ghcSpan (DiagnosticSpan start end) = do
        let srcLocStart =
                mkSrcLoc (fromString start.filename) start.line start.column
            srcLocEnd =
                mkSrcLoc (fromString end.filename) end.line end.column
        mkSrcSpan srcLocStart srcLocEnd

    ghcMsg =
        -- TODO this needs CPP to work with other GHC versions
        -- like: https://github.com/guibou/PyF/blob/main/src/PyF/Internal/QQ.hs#L258
        TcRnUnknownMessage
            . UnknownDiagnostic
            . mkPlainError noHints
            . text
            . T.unpack
            $ msg

    -- \| Stolen from: https://github.com/guibou/PyF
    -- Which comes from:
    -- https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/
    -- Like PyF, we keep it for errors only, so it "should be fine".
    -- TODO check if it is fine with warnings too, when we have them.
    unsafeRunTcM :: TcM a -> Q a
    unsafeRunTcM m = Q (unsafeCoerce m)
