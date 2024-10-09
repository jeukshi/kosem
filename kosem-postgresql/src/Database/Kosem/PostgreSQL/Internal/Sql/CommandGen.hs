{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Sql.CommandGen where

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
import Data.Bifunctor (first, second)
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
import Database.Kosem.PostgreSQL.Internal.Sql.Parser (parse)
import Database.Kosem.PostgreSQL.Internal.Sql.TH
import Database.Kosem.PostgreSQL.Internal.Sql.Typechecker qualified as Typechecker
import Database.Kosem.PostgreSQL.Internal.Sql.Types
import Database.Kosem.PostgreSQL.Internal.Sql.Types qualified
import Database.Kosem.PostgreSQL.Internal.Types
import Debug.Trace
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
import Language.Haskell.TH.Syntax (Loc (..), Q (Q), location, runQ)
import Text.Megaparsec qualified as Megaparsec
import Text.Pretty.Simple (pPrint, pShow)
import Unsafe.Coerce (unsafeCoerce)

data CommandGenEnv e = CommandGenEnv
    { commandPartsS :: State [Lazy.ByteString] e
    , usedParametersS :: State [Parameter] e
    , choicesS :: State [Choice] e
    , underGuardS :: State (Maybe Guard) e
    , compileException :: Exception CompileError e
    }

data CommandFragment
    = CfString Lazy.ByteString
    | CfParam Parameter
    | CfGuard Guard
    | CfGuardEnd Guard
    deriving (Show)

run
    :: (e :> es)
    => Exception CompileError e
    -> CommandInfo
    -> Eff es (CommandVariant [CommandParameter] ByteString)
run ex commandInfo = do
    let commandFragments =
            splitCommand commandInfo.rawCommand commandInfo.input
    rewriteQuery ex commandFragments

runCommandGenEnv
    :: (e :> es)
    => Exception CompileError e
    -> [Lazy.ByteString]
    -> [Parameter]
    -> [Choice]
    -> Maybe Guard
    -> (forall e. CommandGenEnv e -> Eff (e :& es) r)
    -> Eff es r
runCommandGenEnv ex commandParts parameters choices mbGuard action = do
    withStateSource \stateSource -> do
        commandPartsS <- newState stateSource commandParts
        usedParametersS <- newState stateSource parameters
        choicesS <- newState stateSource choices
        underGuardS <- newState stateSource mbGuard
        useImplIn
            action
            CommandGenEnv
                { commandPartsS = mapHandle commandPartsS
                , usedParametersS = mapHandle usedParametersS
                , choicesS = mapHandle choicesS
                , underGuardS = mapHandle underGuardS
                , compileException = mapHandle ex
                }

rewriteQuery
    :: (e :> es)
    => Exception CompileError e
    -> [CommandFragment]
    -> Eff es (CommandVariant [CommandParameter] ByteString)
rewriteQuery ex commandFragments =
    runCommandGenEnv ex [] [] [] Nothing \(env :: CommandGenEnv e) -> do
        (res, _) <- yieldToReverseList \y -> do
            go env y commandFragments
        toCommandVariant ex res
  where
    go
        :: (e1 :> es, e2 :> es)
        => CommandGenEnv e1
        -> Stream (ByteString, [CommandParameter], [Choice]) e2
        -> [CommandFragment]
        -> Eff es ()
    go env y = \cases
        ((CfString bs) : cs) -> do
            modify env.commandPartsS (<> [bs])
            go env y cs
        ((CfParam parameter) : cs) -> do
            paramNo <- getParamNumber env.usedParametersS parameter
            modify env.commandPartsS (<> [LBS.pack ("$" <> show paramNo)])
            go env y cs
        ((CfGuard guard) : cs) -> do
            choices <- get env.choicesS
            case choiceAlreadyMade guard.gIdentifier choices of
                (Just (VcBool True)) -> do
                    put env.underGuardS $ Just guard
                    go env y cs
                (Just (VcMaybe True)) -> do
                    put env.underGuardS $ Just guard
                    go env y cs
                (Just (VcBool False)) -> go env y (skipGuard cs)
                (Just (VcMaybe False)) -> go env y (skipGuard cs)
                Nothing -> do
                    commandParts <- get env.commandPartsS
                    usedParameters <- get env.usedParametersS
                    runCommandGenEnv
                        env.compileException
                        commandParts
                        usedParameters
                        choices
                        (Just guard)
                        \(newEnv :: CommandGenEnv eN) -> do
                            modify @eN newEnv.choicesS (<> [guardToChoice guard True])
                            go newEnv y cs
                    modify env.choicesS (<> [guardToChoice guard False])
                    go env y (skipGuard cs)
        ((CfGuardEnd guard) : cs) -> do
            put env.underGuardS Nothing
            go env y cs
        [] -> do
            commandParts <- get env.commandPartsS
            let commandBuilder = foldMap Builder.lazyByteString commandParts
            let commandBs = toStrict $ Builder.toLazyByteString commandBuilder
            parameters <- map paramForTH <$> get env.usedParametersS
            choices <- get env.choicesS
            yield y (commandBs, parameters, choices)
      where
        skipGuard :: [CommandFragment] -> [CommandFragment]
        skipGuard =
            drop 1 . dropWhile \case
                (CfGuardEnd _) -> False
                _ -> True

        choiceAlreadyMade
            :: Identifier
            -> [Choice]
            -> Maybe ChoiceOption
        choiceAlreadyMade identifier choices =
            case find ((== identifier) . choiceIdentifier) choices of
                Nothing -> Nothing
                (Just choice) -> Just choice.choiceOption

        getParamNumber
            :: (e :> es)
            => State [Parameter] e
            -> Parameter
            -> Eff es Int
        getParamNumber usedParamsS param = do
            usedParams <- get usedParamsS
            case find Nothing usedParams param of
                Nothing -> do
                    put usedParamsS $ usedParams <> [param]
                    return $ length usedParams + 1
                Just n -> return n
          where
            find :: Maybe Int -> [Parameter] -> Parameter -> Maybe Int
            find Nothing (p : ps) parameter =
                if p.pIdentifier /= parameter.pIdentifier
                    then find (Just 2) ps parameter
                    else Just 1
            find (Just i) (p : ps) parameter =
                if p.pIdentifier /= parameter.pIdentifier
                    then find (Just $ i + 1) ps parameter
                    else Just i
            find _ [] _ = Nothing

        paramForTH :: Parameter -> CommandParameter
        paramForTH p =
            MkCommandParameter
                { cpIdentifier = p.pIdentifier
                , cpHsType = (.hsType) . fromJust $ p.info
                , cpIsNullable = (.nullable) . fromJust $ p.info
                }

        guardToChoice :: Guard -> Bool -> Choice
        guardToChoice guard bool = case guard.guardType of
            BooleanGuard -> Choice guard.gIdentifier (VcBool bool)
            MaybeGuard -> Choice guard.gIdentifier (VcMaybe bool)

toCommandVariant
    :: (e :> es)
    => Exception CompileError e
    -> [(ByteString, [CommandParameter], [Choice])]
    -> Eff es (CommandVariant [CommandParameter] ByteString)
toCommandVariant ex = \cases
    [] -> throw ex (AssertionError "TODO no command")
    [(bs, cps, choices)] -> do
        unless (null choices) do
            throw ex (AssertionError "TODO choices in single command")
        return $ SingleCommand (MkCommand cps bs)
    [(bs, cps, [choice]), (bs2, cps2, [choice2])] -> do
        unless (choice.choiceIdentifier == choice2.choiceIdentifier) do
            throw ex (AssertionError "TODO both choices dont have the same id")
        unless (choicesOptionsMatch choice.choiceOption choice2.choiceOption) do
            throw ex (AssertionError "TODO both choices don't have the same constructor or different values")
        return $
            TwoCommands
                choice.choiceIdentifier
                choice.choiceOption
                (MkCommand cps bs)
                choice2.choiceOption
                (MkCommand cps2 bs2)
    xs ->
        return $
            MultipleCommands $
                map
                    ( \(bs, cps, choices) ->
                        (choices, MkCommand cps bs)
                    )
                    xs

choicesOptionsMatch :: ChoiceOption -> ChoiceOption -> Bool
choicesOptionsMatch = \cases
    (VcBool b1) (VcBool b2) -> not b1 == b2
    (VcMaybe b1) (VcMaybe b2) -> not b1 == b2
    (VcMaybe _) (VcBool _) -> False
    (VcBool _) (VcMaybe _) -> False

splitCommand
    :: String
    -> [CommandInput]
    -> [CommandFragment]
splitCommand command inputs = runPureEff do
    (res, _) <- yieldToReverseList \y -> do
        go y command 0 inputs
    return $ map snd . sortOn fst $ res
  where
    go
        :: (e :> es)
        => Stream (Int, CommandFragment) e
        -> String
        -> Int
        -> [CommandInput]
        -> Eff es ()
    go y command offset = \cases
        ((CommandParameter parameter) : cs) -> do
            let start = unP parameter.position
            let paramLen = parameterLength parameter
            let (keep, end) = splitAt (start - offset) command
            let remaining = drop paramLen end
            unless (null keep) do yield y (offset, CfString $ stringToBs keep)
            yield y (start, CfParam parameter)
            go y remaining (start + paramLen) cs
        ((CommandGuard guard) : cs) -> do
            let (keep, end) = splitAt (unP guard.guardPos - offset) command
            let offsetKeep =
                    offset + (unP guard.guardPos - offset)
            -- \| We use + 1 to remove the opening bracket `{`.
            let remaining = drop (unP guard.openBracketPos - offsetKeep + 1) end
            let offsetRemaining =
                    offsetKeep + (unP guard.openBracketPos - offsetKeep + 1)
            let (underGuardCommand, pastGuardCommand) =
                    splitAt (unP guard.closeBracketPos - offsetRemaining) remaining
            let offsetPast =
                    offsetRemaining + (unP guard.closeBracketPos - offsetRemaining)
            let (underGuardCi, pastGuardCi) =
                    splitByPos cs offsetPast
            unless (null keep) do yield y (offset, CfString $ stringToBs keep)
            yield y (unP guard.guardPos, CfGuard guard)
            yield y (unP guard.closeBracketPos, CfGuardEnd guard)
            go y underGuardCommand offsetRemaining underGuardCi
            -- \| We drop first char, which is closing bracket `}`
            -- and move offset by + 1 for that reason.
            go y (drop 1 pastGuardCommand) (offsetPast + 1) pastGuardCi
        [] -> do
            unless (null command) do yield y (offset, CfString $ stringToBs command)
    stringToBs :: String -> Lazy.ByteString
    stringToBs = Builder.toLazyByteString . Builder.stringUtf8
    parameterLength :: Parameter -> Int
    parameterLength parameter =
        identifierLength parameter.pIdentifier + T.length (parameterTypeToText parameter.paramType)
    guardLength :: Guard -> Int
    guardLength guard =
        identifierLength guard.gIdentifier + T.length (guardTypeToText guard.guardType)
    splitByPos :: [CommandInput] -> Int -> ([CommandInput], [CommandInput])
    splitByPos ci pos = do
        let less = filter (\c -> unP (commandInputPosition c) < pos) ci
        let greater = filter (\c -> unP (commandInputPosition c) >= pos) ci
        (less, greater)
