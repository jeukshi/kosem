{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Internal.Sql.Typechecker where

import Bluefin.Eff (runPureEff)
import Bluefin.Exception (try)
import Data.List.NonEmpty qualified as NE
import Database.Kosem.PostgreSQL.Internal.Diagnostics (CompileError)
import Database.Kosem.PostgreSQL.Internal.PgBuiltin
import Database.Kosem.PostgreSQL.Internal.PgType qualified as PgType
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
                , NE.singleton (co "" PgType.Unknown Nullable)
                )
    isRight `shouldBe` "tcOK"
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
    describe "'unknown' in select" $ do
        it "is casted as text" do
            res <- tcOk [text| select 'abc' text  |]
            res `shouldBe` NE.singleton (co "text" PgType.Text NonNullable)

    describe "binary operators" $ do
        it "one arg is unknown" do
            res <- tcOk [text| select 'abc'::text || 'cba' r  |]
            res `shouldBe` NE.singleton (co "r" PgType.Text NonNullable)
        it "both args are 'unknown'" do
            pendingWith "TODO"
            res <- tcOk [text| select 'abc' || 'cba' r  |]
            res `shouldBe` NE.singleton (co "r" PgType.Text NonNullable)

    describe "integer literals" $ do
        it "n = 123 is `integer`" do
            res <- tcOk [text| select 123 r |]
            res `shouldBe` NE.singleton (co "r" PgType.Integer NonNullable)
        it "n = -123 is `integer`" do
            res <- tcOk [text| select -123 r |]
            res `shouldBe` NE.singleton (co "r" PgType.Integer NonNullable)
        it "n > 2147483647 (max for `integer) is `bigint`" do
            res <- tcOk [text| select 2147483648 r |]
            res `shouldBe` NE.singleton (co "r" PgType.Bigint NonNullable)
        it "n > 9223372036854775807 (max for `bigint`) is `numeric`" do
            res <- tcOk [text| select 9223372036854775808 r |]
            res `shouldBe` NE.singleton (co "r" PgType.Numeric NonNullable)
        it "n = -2147483648 (min for `integer`) is `integer`" do
            res <- tcOk [text| select -2147483648 r |]
            res `shouldBe` NE.singleton (co "r" PgType.Integer NonNullable)
            pendingWith "see comment"
        it "n = -2147483649 is `bigint`" do
            res <- tcOk [text| select -2147483649 r |]
            res `shouldBe` NE.singleton (co "r" PgType.Bigint NonNullable)
        it "n > -9223372036854775808 (min for `bigint`) is `bigint`" do
            res <- tcOk [text| select -9223372036854775808 r |]
            res `shouldBe` NE.singleton (co "r" PgType.Bigint NonNullable)
        it "n > -9223372036854775809 is `numeric`" do
            res <- tcOk [text| select -9223372036854775809 r |]
            res `shouldBe` NE.singleton (co "r" PgType.Numeric NonNullable)
        it "select (- (-2147483648)) is `integer`" do
            -- \| Postgres is even smarter with literals, as above works
            -- but this doesn't:
            -- select -(-2147483648::integer)
            -- select -(-2147483648)::integer
            pendingWith "read comment and cry"
