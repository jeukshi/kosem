{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Sql where

import Bluefin.Compound
import Bluefin.Coroutine
import Bluefin.Eff (Eff, runEff, runPureEff, (:&), (:>))
import Bluefin.Exception
import Bluefin.IO (effIO)
import Bluefin.Jump (jumpTo, withJump)
import Bluefin.State (State, get, modify, put)
import Bluefin.StateSource (newState, withStateSource)
import Bluefin.Stream (Stream, inFoldable, yield, yieldToList, yieldToReverseList)
import Control.Monad (replicateM, unless, when)
import Data.Bifunctor (Bifunctor (bimap), first, second)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Either (partitionEithers)
import Data.Foldable (fold, for_)
import Data.Functor (void)
import Data.List (find, foldl', nub, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable (for)
import Database.Kosem.PostgreSQL.Internal.Diagnostics (CompileError (..), compilationError)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.P (P (MkP), unP)
import Database.Kosem.PostgreSQL.Internal.PgBuiltin (DatabaseConfig (..), defaultDatabaseConfig)
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.Row qualified
import Database.Kosem.PostgreSQL.Internal.Sql.Ast
import Database.Kosem.PostgreSQL.Internal.Sql.CommandGen qualified as CommandGen
import Database.Kosem.PostgreSQL.Internal.Sql.Parser qualified as Parser
import Database.Kosem.PostgreSQL.Internal.Sql.TH
import Database.Kosem.PostgreSQL.Internal.Sql.Typechecker qualified as Typechecker
import Database.Kosem.PostgreSQL.Internal.Sql.Types
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
import Text.Pretty.Simple (pPrint, pShow)
import Unsafe.Coerce (unsafeCoerce)

unsafeSql :: Database -> String -> Q Exp
unsafeSql database userInput = do
    let res = runPureEff do
            try \ex -> do
                ast <- Parser.run ex userInput
                commandInfo <-
                    Typechecker.run ex database ast userInput
                -- TODO make this a proper record
                stuff <- CommandGen.run ex commandInfo
                return (stuff, commandInfo)
    case res of
        Right (stuff, ci) -> gen stuff ci
        Left err -> do
            compilationError userInput err
  where
    gen :: CommandVariant [CommandParameter] ByteString -> CommandInfo -> Q Exp
    gen commands commandInfo = do
        commandsQ <-
            traverse genCommand
                . first genParamsList
                $ commands
        let result = commandInfo.output
        let numberOfColumns = length commandInfo.output
        [e|
            SqlCommand
                { statement = $(genPatternMatch cCommand commandsQ)
                , columnsNumber = numberOfColumns
                , rowProto = Row [] :: $(genRowType result)
                , rowParser = $(genRowParser result)
                , params = $(genPatternMatch cParams commandsQ)
                }
            |]
