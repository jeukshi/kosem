{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.Diagnostics where

import Data.Coerce (coerce)
import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC (mkSrcSpan)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Parser.Errors.Types (PsMessage (..))
import GHC.Tc.Errors.Types (TcRnMessage (TcRnUnknownMessage))
import GHC.Tc.Types (TcM)
import GHC.Tc.Utils.Monad (addErrAt)
import GHC.Types.Error (mkPlainError, noHints)
import GHC.Types.SrcLoc (SrcSpan, mkSrcLoc)
import GHC.Utils.Outputable (text)
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (Exp, Loc (..), Q (Q), location)
import Text.Megaparsec (PosState (..), TraversableStream (reachOffsetNoLine))
import Text.Megaparsec.Pos (SourcePos (..), mkPos, unPos)
import Unsafe.Coerce (unsafeCoerce)

initPosState :: Text -> PosState Text
initPosState input =
    PosState
        { pstateInput = input
        , pstateOffset = 0
        , pstateSourcePos =
            SourcePos
                { sourceName = ""
                , sourceLine = mkPos 1
                , sourceColumn = mkPos 1
                }
        , pstateTabWidth = mkPos 1
        , pstateLinePrefix = ""
        }

newtype P = MkP {unP :: Int}
    deriving (Show, Eq)

data CompileError
    = ParseError P String
    | TypeError P String
    | NoAliasError P String
    | ParametersWithoutCastError P String
    | ExprWithNoAlias P String
    deriving (Show)

compileErrorP :: CompileError -> P
compileErrorP = \case
    ParseError p _ -> p
    TypeError p _ -> p
    NoAliasError p _ -> p
    ParametersWithoutCastError p _ -> p
    ExprWithNoAlias p _ -> p

compileErrorMsg :: CompileError -> String
compileErrorMsg = \case
    ParseError _ msg -> msg
    TypeError _ msg -> msg
    NoAliasError _ msg -> msg
    ParametersWithoutCastError _ msg -> msg
    ExprWithNoAlias _ msg -> msg

compileError :: Text -> CompileError -> Q Exp
compileError input error = do
    let p = compileErrorP error
    let msg = compileErrorMsg error
    qqLoc <- location
    let (qqLine, qqColumn) = loc_start qqLoc
        qqFilename = fromString . loc_filename $ qqLoc
        errSourcePos =
            pstateSourcePos $
                reachOffsetNoLine (coerce p) (initPosState input)
        errLine = unPos errSourcePos.sourceLine
        errColumn = unPos errSourcePos.sourceColumn
        srcLine = qqLine + errLine - 1
        srcColumn =
            if errLine == 1
                then -- \| In QuasiQuotes every line starts at column == 1,
                -- except the first line, which starts at arbitrary position.
                    errColumn + qqColumn - 1
                else errColumn
        srcLocStart =
            mkSrcLoc qqFilename srcLine srcColumn
        srcLocEnd =
            mkSrcLoc qqFilename srcLine srcColumn
        srcSpan = mkSrcSpan srcLocStart srcLocEnd
    errorWithSpan srcSpan msg
    -- \| Return any 'Exp', so GHC can type-check this function.
    -- We report an error, so it doesn't matter.
    [e|()|]

errorWithSpan :: SrcSpan -> String -> Q ()
errorWithSpan loc msg = unsafeRunTcM $ addErrAt loc msg'
  where
    msg' =
        TcRnUnknownMessage
            ( GhcPsMessage $
                PsUnknownMessage $
                    mkPlainError noHints $
                        text msg
            )
    -- \| Stolen from: https://github.com/guibou/PyF
    -- Which comes from:
    -- https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/
    -- Like PyF, we keep it for errors only, so it "should be fine".
    -- TODO check if it is fine with warnings too, when we have them.
    unsafeRunTcM :: TcM a -> Q a
    unsafeRunTcM m = Q (unsafeCoerce m)
