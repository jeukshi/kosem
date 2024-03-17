{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.Diagnostics (
    initPosState,
    P (..),
    movePby,
    DiagnosticSpan (..),
    combineSpans,
    CompileError (..),
    compileError,
)
where

import Data.Coerce (coerce)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Diagnostics.GHC (
    DiagnosticSpan (..),
    SourcePoint (..),
    errorWithSpan,
 )
import GHC (mkSrcSpan)
import GHC.Tc.Types (TcM)
import GHC.Types.SrcLoc (SrcSpan, mkSrcLoc)
import Language.Haskell.TH.Syntax (Exp, Loc (..), Q (Q), location)
import Text.Megaparsec (PosState (..), TraversableStream (reachOffsetNoLine))
import Text.Megaparsec.Pos (SourcePos (..), mkPos, unPos)
import GHC.Data.FastString (FastString)

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

-- TODO explain P
newtype P = MkP {unP :: Int}
    deriving (Show, Eq, Ord)

movePby :: P -> Int -> P
movePby p int = coerce $ coerce p + int

combineSpans
    :: DiagnosticSpan P
    -> DiagnosticSpan P
    -> DiagnosticSpan P
combineSpans
    (DiagnosticSpan p1_min p1_max)
    (DiagnosticSpan p2_min p2_max) =
        DiagnosticSpan
            (min p1_min p2_min)
            (max p1_max p2_max)

spanWithCodePoint
    :: String
    -> (Int, Int)
    -> Text
    -> DiagnosticSpan P
    -> DiagnosticSpan SourcePoint
spanWithCodePoint
    filename
    qqPoint
    input
    (DiagnosticSpan pStart pEnd) = do
        let errSourcePosStart =
                pstateSourcePos $
                    reachOffsetNoLine (coerce pStart) (initPosState input)
            errSourcePosEnd =
                pstateSourcePos $
                    reachOffsetNoLine (coerce pEnd) (initPosState input)
        DiagnosticSpan
            (toSourcePoint filename qqPoint errSourcePosStart)
            (toSourcePoint filename qqPoint errSourcePosEnd)
      where
        toSourcePoint :: String -> (Int, Int) -> SourcePos -> SourcePoint
        toSourcePoint filename (qqLine, qqColumn) errSourcePos = do
            let errLine = unPos errSourcePos.sourceLine
                errColumn = unPos errSourcePos.sourceColumn
            SourcePoint
                { line = qqLine + errLine - 1
                , -- \| In QuasiQuotes every line starts at column == 1,
                  -- except the first line, which starts at arbitrary position.
                  column =
                    if errLine == 1
                        then errColumn + qqColumn - 1
                        else errColumn
                , filename = filename
                }

data CompileError
    = ParseError (DiagnosticSpan P) Text
    | TypeError (DiagnosticSpan P) Text
    | NoAliasError (DiagnosticSpan P) Text
    | ParametersWithoutCastError (DiagnosticSpan P) Text
    | ExprWithNoAlias (DiagnosticSpan P) Text
    | OperatorDoesntExist (DiagnosticSpan P) Text
    | ColumnDoesNotExist (DiagnosticSpan P) Text
    | ColumnNameIsAmbigious (DiagnosticSpan P) Text
    | TableDoesNotExist (DiagnosticSpan P) Text
    | TableNameIsAmbigious (DiagnosticSpan P) Text
    deriving (Show)

compileErrorSpan :: CompileError -> DiagnosticSpan P
compileErrorSpan = \case
    ParseError span _ -> span
    TypeError span _ -> span
    NoAliasError span _ -> span
    ParametersWithoutCastError span _ -> span
    ExprWithNoAlias span _ -> span
    OperatorDoesntExist span _ -> span
    ColumnDoesNotExist span _ -> span
    ColumnNameIsAmbigious span _ -> span
    TableDoesNotExist span _ -> span
    TableNameIsAmbigious span _ -> span


compileErrorMsg :: CompileError -> Text
compileErrorMsg = \case
    ParseError _ msg -> msg
    TypeError _ msg -> msg
    NoAliasError _ msg -> msg
    ParametersWithoutCastError _ msg -> msg
    ExprWithNoAlias _ msg -> msg
    OperatorDoesntExist _ msg -> msg
    ColumnDoesNotExist _ msg -> msg
    ColumnNameIsAmbigious _ msg -> msg
    TableDoesNotExist _ msg -> msg
    TableNameIsAmbigious _ msg -> msg

compileError :: Text -> CompileError -> Q Exp
compileError input error = do
    let diagnosticSpan = compileErrorSpan error
    let msg = compileErrorMsg error
    qqLoc <- location
    let qqPoint = loc_start qqLoc
        -- (qqFilename :: String) = fromString . loc_filename $ qqLoc
        span = spanWithCodePoint qqLoc.loc_filename qqPoint input diagnosticSpan
    errorWithSpan span msg
    -- \| Return any 'Exp', so GHC can type-check this function.
    -- We report an error, so it doesn't matter.
    [e|()|]
