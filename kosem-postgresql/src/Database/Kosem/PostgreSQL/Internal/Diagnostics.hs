{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.Diagnostics where

import Data.Coerce (coerce)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Diagnostics.GHC (errorWithSpan)
import GHC (mkSrcSpan)
import GHC.Tc.Types (TcM)
import GHC.Types.SrcLoc (SrcSpan, mkSrcLoc)
import Language.Haskell.TH.Syntax (Exp, Loc (..), Q (Q), location)
import Text.Megaparsec (PosState (..), TraversableStream (reachOffsetNoLine))
import Text.Megaparsec.Pos (SourcePos (..), mkPos, unPos)

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
        -- TODO move all SrcSpan business to Diagnostics.GHC
        srcLocStart =
            mkSrcLoc qqFilename srcLine srcColumn
        srcLocEnd =
            mkSrcLoc qqFilename srcLine srcColumn
        srcSpan = mkSrcSpan srcLocStart srcLocEnd
    errorWithSpan srcSpan msg
    -- \| Return any 'Exp', so GHC can type-check this function.
    -- We report an error, so it doesn't matter.
    [e|()|]
