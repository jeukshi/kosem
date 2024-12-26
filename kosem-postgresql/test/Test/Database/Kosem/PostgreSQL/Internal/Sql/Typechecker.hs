{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Internal.Sql.Typechecker where

import Bluefin.Eff (runPureEff)
import Bluefin.Exception (try)
import Data.List.NonEmpty qualified as NE
import Database.Kosem.PostgreSQL.Internal.Diagnostics (CompileError)
import Database.Kosem.PostgreSQL.Internal.PgBuiltin
import Database.Kosem.PostgreSQL.Internal.Sql.Parser qualified as Parser
import Database.Kosem.PostgreSQL.Internal.Sql.Typechecker (tcExpr)
import Database.Kosem.PostgreSQL.Internal.Sql.Typechecker qualified as Typechecker
import Database.Kosem.PostgreSQL.Internal.Sql.Types (
    CommandInfo (..),
    CommandOutput (..),
 )
import Database.Kosem.PostgreSQL.Internal.Types (
    Database,
    Identifier,
    IsNullable (..),
    PgType,
    SqlMapping,
 )
import Test.Db (emptyDatabase)
import Test.Hspec
import Test.TH (text)

tcOk :: String -> IO (NE.NonEmpty CommandOutput)
tcOk userInput = do
    let res = typecheck emptyDatabase userInput
    let (isRight, commandOutput) = case res of
            (Right co) -> ("tcOK", co.output)
            (Left err) ->
                ( "tcOK: Left" <> show err
                , NE.singleton (co "" PgUnknown Nullable)
                )
    isRight `shouldBe` "OK"
    return commandOutput
  where
    typecheck
        :: Database
        -> String
        -> Either CompileError (CommandInfo CommandOutput)
    typecheck database userInput = do
        runPureEff do
            try \ex -> do
                ast <- Parser.run ex userInput
                Typechecker.run ex database ast userInput

co :: Identifier -> PgType -> IsNullable -> CommandOutput
co = MkCommandOutput

spec :: SpecWith ()
spec = do
    describe "TODO" $ do
        it "TODO" do
            res <- tcOk [text| select 'abc'  |]
            res `shouldBe` NE.singleton (co "text" PgText Nullable)
