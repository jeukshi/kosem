{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Sql where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.List (foldl', sortOn)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable (for)
import Database.Kosem.PostgreSQL.Internal.Diagnostics (CompileError (..), compileError)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.Row qualified
import Database.Kosem.PostgreSQL.Internal.Sql.Ast
import Database.Kosem.PostgreSQL.Internal.Sql.Env (runProgram)
import Database.Kosem.PostgreSQL.Internal.Sql.Parser (parse)
import Database.Kosem.PostgreSQL.Internal.Sql.TH
import Database.Kosem.PostgreSQL.Internal.Sql.Typechecker as Typechecker
import Database.Kosem.PostgreSQL.Internal.Sql.Types (
    CommandInfo (..),
    Parameter,
    ParameterType (..),
    SqlCommand (..),
    parameterTypeToText,
 )
import Database.Kosem.PostgreSQL.Internal.Sql.Types qualified
import Database.Kosem.PostgreSQL.Internal.Types
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Parser.Errors.Types (PsMessage (PsUnknownMessage))
import GHC.Tc.Errors.Types (TcRnMessage (..))
import GHC.Tc.Types (TcM)
import GHC.Tc.Utils.Monad (addErrAt)
import GHC.Types.Error (mkPlainError)
import GHC.Types.SrcLoc (SrcLoc (RealSrcLoc), SrcSpan, mkRealSrcLoc, mkSrcLoc, mkSrcSpan)
import GHC.Utils.Error (noHints)
import GHC.Utils.Outputable (text)
import Language.Haskell.TH (Exp, Loc (loc_filename), Name, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Loc (..), Q (Q), location)
import Text.Megaparsec qualified as Megaparsec
import Unsafe.Coerce (unsafeCoerce)
import Debug.Trace (trace, traceShow)

unsafeSql :: Database -> String -> Q Exp
unsafeSql database userInputString = do
    let userInput = T.pack userInputString
    case Typechecker.run database userInput of
        Right commandInfo -> do
            let numberOfColumns = length commandInfo.output
                result = commandInfo.output
                params =
                    map (\p -> (p.identifier, p.hsType, p.nullable))
                        . sortOn (.number)
                        $ commandInfo.input
                command =
                    T.encodeUtf8 $
                        replaceParams
                            commandInfo.input
                            commandInfo.rawCommand
            [e|
                SqlCommand
                    { statement = command
                    , columnsNumber = numberOfColumns
                    , rowProto = Row [] :: $(genRowType result)
                    , rowParser = $(genRowParser result)
                    , params = $(genParamsList params)
                    }
                |]
        Left e -> do
            compileError userInput e
  where
    replaceParams :: [Parameter] -> Text -> Text
    replaceParams parameters rawCommand = do
        let replacements =
                map
                    ( \param ->
                        ( parameterTypeToText param.paramType
                            <> identifierToText param.identifier
                        , T.pack ("$" <> show param.number)
                        )
                    )
                    parameters
        -- FIXME this will replace in comments too
        traceShow replacements foldl'
            (\acc (old, new) -> T.replace old new acc)
            rawCommand
            replacements
