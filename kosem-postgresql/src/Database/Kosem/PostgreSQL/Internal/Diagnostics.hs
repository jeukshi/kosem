{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Database.Kosem.PostgreSQL.Internal.Diagnostics (
    P (..),
    DiagnosticSpan (..),
    combineSpans,
    CompileError (..),
    compilationError,
)
where

import Data.Coerce (coerce)
import Data.List (intercalate, intersperse)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.Diagnostics.GHC (
    DiagnosticSpan (..),
    SourcePoint (..),
    errorWithSpan,
 )
import Database.Kosem.PostgreSQL.Internal.P (P (MkP, unP), initPosState, movePby)
import Database.Kosem.PostgreSQL.Internal.PgBuiltin
import Database.Kosem.PostgreSQL.Internal.PgType qualified as PgType
import Database.Kosem.PostgreSQL.Internal.Sql.Ast (Expr (..), LiteralValue (..))
import Database.Kosem.PostgreSQL.Internal.Types (
    Alias,
    Identifier,
    Operator,
    PgType,
    TypeInfo,
    aliasedIdentifierPretty,
    hsIdentifierLength,
    identifierLength,
    identifierPretty,
    identifierToString,
    operatorLength,
    operatorPretty,
    pgTypePretty,
 )
import GHC.Data.FastString (FastString)
import GHC.Tc.Types (TcM)
import GHC.Types.SrcLoc (SrcSpan, mkSrcLoc)
import Language.Haskell.TH.Syntax (Exp, Loc (..), Q (Q), location)
import Text.Megaparsec (PosState (..), TraversableStream (reachOffsetNoLine))
import Text.Megaparsec.Pos (SourcePos (..), mkPos, unPos)

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
    -> String
    -> DiagnosticSpan P
    -> DiagnosticSpan SourcePoint
spanWithCodePoint
    filename
    qqPoint
    input
    (DiagnosticSpan pStart pEnd) = do
        let errSourcePosStart =
                pstateSourcePos $
                    reachOffsetNoLine (unP pStart) (initPosState input)
            errSourcePosEnd =
                pstateSourcePos $
                    reachOffsetNoLine (unP pEnd) (initPosState input)
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
    = AssertionError String
    | ParseError P String
    | ArgumentTypeError (Expr TypeInfo) String PgType
    | ConditionTypeError (Expr TypeInfo) String
    | ParameterWithoutCastError P Identifier
    | MaybeParameterWithoutCastError P Identifier
    | FunctionDoesNotExist P Identifier [PgType]
    | ExprWithNoAlias (Expr TypeInfo)
    | OperatorDoesntExist P PgType Operator PgType
    | UnaryOperatorDoesntExist P Operator PgType
    | ColumnDoesNotExist P (Maybe Alias) Identifier
    | ColumnNameIsAmbiguous P Identifier
    | TableDoesNotExist P Identifier
    | TableNameIsAmbiguous P Identifier
    deriving (Show)

compileErrorSpan :: CompileError -> DiagnosticSpan P
compileErrorSpan = \case
    AssertionError s -> DiagnosticSpan (MkP 0) (MkP 1) -- FIXME ??
    ParseError p _ ->
        DiagnosticSpan p p
    ArgumentTypeError expr _ _ -> toDiagnosticSpan expr
    ConditionTypeError expr _ -> toDiagnosticSpan expr
    ParameterWithoutCastError p identifier ->
        DiagnosticSpan
            p
            -- \| +1 from ':' prefix.
            (p `movePby` (identifierLength identifier + 1))
    MaybeParameterWithoutCastError p identifier ->
        DiagnosticSpan
            p
            -- \| +2 from ':?' prefix.
            (p `movePby` (identifierLength identifier + 2))
    FunctionDoesNotExist p identifier _ ->
        DiagnosticSpan
            p
            (p `movePby` identifierLength identifier)
    ExprWithNoAlias expr -> toDiagnosticSpan expr
    OperatorDoesntExist p _ operator _ ->
        (DiagnosticSpan p (p `movePby` operatorLength operator))
    UnaryOperatorDoesntExist p operator _ ->
        (DiagnosticSpan p (p `movePby` operatorLength operator))
    ColumnDoesNotExist p mbAlias identifier -> do
        let aliasLength =
                -- \| +1 from dot.
                maybe 0 ((+ 1) . identifierLength) mbAlias
        DiagnosticSpan p (p `movePby` (aliasLength + identifierLength identifier))
    ColumnNameIsAmbiguous p identifier ->
        DiagnosticSpan p (p `movePby` identifierLength identifier)
    TableDoesNotExist p identifier ->
        DiagnosticSpan p (p `movePby` identifierLength identifier)
    TableNameIsAmbiguous p identifier ->
        DiagnosticSpan p (p `movePby` identifierLength identifier)

compileErrorMsg :: CompileError -> String
compileErrorMsg = \case
    AssertionError msg -> msg
    ParseError _ msg -> msg
    ArgumentTypeError _ func ty ->
        "argument of ‘" <> func <> "’ must be type " <> pgTypePretty ty
    ConditionTypeError _ msg ->
        "argument of ‘" <> msg <> "’ must be of type " <> pgTypePretty PgType.Boolean
    ParameterWithoutCastError _ _ ->
        "parameters without cast are not supported"
    MaybeParameterWithoutCastError _ _ ->
        "parameters without cast are not supported"
    FunctionDoesNotExist _ name argTys ->
        "function "
            <> identifierToString name
            <> "("
            <> (intercalate ", " . map pgTypePretty $ argTys)
            <> ")"
            <> " does not exist"
    ExprWithNoAlias _ -> "expression does not have an alias"
    OperatorDoesntExist _ lhs op rhs ->
        "operator does not exist: "
            <> pgTypePretty lhs
            <> " "
            <> operatorPretty op
            <> " "
            <> pgTypePretty rhs
    UnaryOperatorDoesntExist _ op rhs ->
        "operator does not exist: "
            <> operatorPretty op
            <> " "
            <> pgTypePretty rhs
    ColumnDoesNotExist _ mbAlias identifier -> do
        let colPretty = case mbAlias of
                Nothing -> identifierPretty identifier
                (Just alias) -> aliasedIdentifierPretty alias identifier
        "column does not exist: " <> colPretty
    ColumnNameIsAmbiguous _ identifier ->
        "column name is ambiguous: " <> identifierPretty identifier
    TableDoesNotExist _ identifier ->
        "table does not exist: " <> identifierPretty identifier
    TableNameIsAmbiguous p identifier ->
        "table name is ambiguous: " <> identifierPretty identifier

toDiagnosticSpan :: Expr a -> DiagnosticSpan P
toDiagnosticSpan = \cases
    (EPgCast p1 _ p2 identifier _) ->
        DiagnosticSpan
            p1
            (p2 `movePby` identifierLength identifier)
    (EParens p1 _ p2 _) ->
        DiagnosticSpan p1 p2
    (EParam p identifier _) ->
        DiagnosticSpan
            p
            -- \| +1 from ':' prefix.
            (p `movePby` (hsIdentifierLength identifier + 1))
    (EParamMaybe p identifier _) ->
        DiagnosticSpan
            p
            -- \| +2 from ':?' prefix.
            (p `movePby` (hsIdentifierLength identifier + 2))
    (EFunction p identifier _ _) ->
        DiagnosticSpan
            p
            (p `movePby` identifierLength identifier)
    (ELiteral p lit _) -> case lit of
        NonIntegerNumberLiteral x -> undefined -- TODO
        IntegerLiteral int ->
            DiagnosticSpan
                p
                (p `movePby` length (show int))
        BoolLiteral text ->
            DiagnosticSpan
                p
                (p `movePby` length text)
        StringLiteral text ->
            DiagnosticSpan
                p
                -- \| +2 from single quote.
                (p `movePby` (length text + 2))
    (ECol p mbAlias identifier _) -> do
        let aliasLength =
                -- \| +1 from dot.
                maybe 0 ((+ 1) . identifierLength) mbAlias
        DiagnosticSpan
            p
            (p `movePby` (aliasLength + identifierLength identifier))
    (ENot p expr) ->
        DiagnosticSpan p p
            `combineSpans` toDiagnosticSpan expr
    (EGuardedBoolAnd _ p1 _ _ _ p2) ->
        DiagnosticSpan p1 p2
    (EGuardedMaybeAnd _ p1 _ _ _ p2) ->
        DiagnosticSpan p1 p2
    (EAnd _ lhs rhs) ->
        toDiagnosticSpan lhs
            `combineSpans` toDiagnosticSpan rhs
    (EOr _ lhs rhs) ->
        toDiagnosticSpan lhs
            `combineSpans` toDiagnosticSpan rhs
    (EBinOp _ lhs _ rhs _) ->
        toDiagnosticSpan lhs
            `combineSpans` toDiagnosticSpan rhs
    (EUnaryOp p _ rhs _) ->
        DiagnosticSpan p p
            `combineSpans` toDiagnosticSpan rhs
    (EBetween _ lhs _ rhs2) ->
        toDiagnosticSpan lhs
            `combineSpans` toDiagnosticSpan rhs2
    (ENotBetween _ lhs _ rhs2) ->
        toDiagnosticSpan lhs
            `combineSpans` toDiagnosticSpan rhs2

compilationError :: String -> CompileError -> Q Exp
compilationError input error = do
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
