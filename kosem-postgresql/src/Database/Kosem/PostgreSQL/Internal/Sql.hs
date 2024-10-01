{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Sql where

import Bluefin.Eff (Eff, runEff, runPureEff, (:>))
import Bluefin.IO (effIO)
import Bluefin.Jump (jumpTo, withJump)
import Bluefin.State (State, get, modify, put)
import Bluefin.StateSource (newState, withStateSource)
import Bluefin.Stream (yield, yieldToList)
import Control.Monad (replicateM)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (toStrict)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.List (find, foldl', nub, sortOn)
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable (for)
import Database.Kosem.PostgreSQL.Internal.Diagnostics (CompileError (..), compilationError)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.P (unP)
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.Row qualified
import Database.Kosem.PostgreSQL.Internal.Sql.Ast
import Database.Kosem.PostgreSQL.Internal.Sql.Env
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

unsafeSql :: Database -> String -> Q Exp
unsafeSql database userInputString = do
    let userInput = T.pack userInputString
    case Typechecker.run database userInput of
        Right commandInfo -> do
            let numberOfColumns = length commandInfo.output
                gTT = guardsTruthTable commandInfo.input
                allInputs =
                    map
                        ( ( \(q, ps, tt) ->
                                (q, map paramForTH ps, tt)
                          )
                            . rewriteQuery commandInfo.input (T.unpack commandInfo.rawCommand)
                        )
                        gTT
                tts = map (\(_, _, t) -> t) allInputs
                commands = map (\(c, _, _) -> c) allInputs
                parameters = map (\(_, p, _) -> p) allInputs
            commandsExp <- traverse genCommand commands
            parametersExp <- traverse genParamsList parameters
            let result = commandInfo.output
            [e|
                SqlCommand
                    { statement = $(genPatternMatch (zip commandsExp tts))
                    , columnsNumber = numberOfColumns
                    , rowProto = Row [] :: $(genRowType result)
                    , rowParser = $(genRowParser result)
                    , params = $(genPatternMatch (zip parametersExp tts))
                    }
                |]
        Left e -> do
            compilationError userInput e
  where
    paramForTH :: Parameter -> (Identifier, Name, IsNullable)
    paramForTH p =
        ( p.identifier
        , (.hsType) . fromJust $ p.info
        , (.nullable) . fromJust $ p.info
        )

data Action
    = Skip Int
    | SkipCloseCurly
    | Take Int Parameter
    | TakeRest
    deriving (Show)

-- FIXME All of this is quite bad.
rewriteQuery
    :: [Parameter]
    -> String
    -> [(Identifier, Bool)]
    -> (ByteString, [Parameter], [(Identifier, Bool)])
rewriteQuery parameters queryString truthTable = runPureEff do
    withStateSource \stateSource -> do
        queryBuilderS <- newState stateSource (mempty :: Builder)
        usedParametersS <- newState stateSource (mempty :: [Parameter])
        nextActionS <- newState stateSource TakeRest
        underGuardS <- newState stateSource False
        nextAction nextActionS 0
        for_ (zip [(0 :: Int) ..] queryString) $ \(ix, char) -> do
            action <- get nextActionS
            ug <- get underGuardS
            case action of
                TakeRest -> do
                    if char == '}' && ug -- FIXME this assumes no '}' in comments or variables, BAD
                        then put underGuardS False
                        else modify queryBuilderS (<> Builder.charUtf8 char)
                Take n currParam ->
                    if char == '}' && ug -- FIXME this assumes no '}' in comments or variables, BAD
                        then put underGuardS False
                        else
                            if n > ix
                                then modify queryBuilderS (<> Builder.charUtf8 char)
                                else
                                    if currParam.paramType /= GuardParameter
                                        then do
                                            no <- getParamNumber usedParametersS currParam
                                            modify queryBuilderS (<> Builder.stringUtf8 ("$" <> show no))
                                            let extraForMaybeParams = if currParam.paramType == SimpleMaybeParameter then 1 else 0
                                            put nextActionS $ Skip (n + identifierLength currParam.identifier + extraForMaybeParams)
                                        else
                                            if shouldTake truthTable currParam.identifier
                                                then do
                                                    put nextActionS $ Skip (n + identifierLength currParam.identifier + 2)
                                                    put underGuardS True
                                                else put nextActionS SkipCloseCurly
                Skip n ->
                    if n > ix
                        then pure ()
                        else nextAction nextActionS n
                SkipCloseCurly ->
                    if char /= '}'
                        then pure ()
                        else nextAction nextActionS ix
        builder <- get queryBuilderS
        finalParams <- get usedParametersS
        let finalCommand = toStrict . Builder.toLazyByteString $ builder
        return (finalCommand, finalParams, truthTable)
  where
    nextAction
        :: (e :> es)
        => State Action e
        -> Int
        -> Eff es ()
    nextAction nextActionS n = case nextParam n parameters of
        Nothing -> put nextActionS TakeRest
        (Just currParam) -> put nextActionS $ Take (unP currParam.position) currParam

shouldTake :: [(Identifier, Bool)] -> Identifier -> Bool
shouldTake tt identifier = case filter (\(i, _) -> i == identifier) tt of
    (x : _) -> snd x
    [] ->
        error $
            "Guard identifier: "
                <> identifierToString identifier
                <> "not in the list: "
                <> show tt

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
        if p.identifier /= parameter.identifier
            then find (Just 2) ps parameter
            else Just 1
    find (Just i) (p : ps) parameter =
        if p.identifier /= parameter.identifier
            then find (Just $ i + 1) ps parameter
            else Just i
    find _ [] _ = Nothing

nextParam
    :: Int
    -> [Parameter]
    -> Maybe Parameter
nextParam n = find (\p -> unP p.position > n)

guardsTruthTable :: [Parameter] -> [[(Identifier, Bool)]]
guardsTruthTable parameters = do
    let guards =
            nub
                . map (.identifier)
                . filter
                    (\param -> param.paramType == GuardParameter)
                $ parameters
    map (zip guards) $ replicateM (length guards) [True, False]
