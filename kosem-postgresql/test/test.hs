import Control.Exception (evaluate)
import Test.Database.Kosem.PostgreSQL.Internal qualified
import Test.Database.Kosem.PostgreSQL.Internal.Sql qualified
import Test.Database.Kosem.PostgreSQL.Internal.Sql.Parser qualified
import Test.Database.Kosem.PostgreSQL.Internal.Sql.Typechecker qualified
import Test.Database.Kosem.PostgreSQL.Internal.ToFromField qualified
import Test.Database.Kosem.PostgreSQL.Schema.Internal.Parser qualified
import Test.Hspec

main :: IO ()
main = hspec spec

spec = do
    describe "Test.Database.Kosem.PostgreSQL.Internal" do
        Test.Database.Kosem.PostgreSQL.Internal.spec
    describe "Test.Database.Kosem.PostgreSQL.Internal.Sql.Parser" do
        Test.Database.Kosem.PostgreSQL.Internal.Sql.Parser.spec
    describe "Test.Database.Kosem.PostgreSQL.Internal.Sql.Typechecker" do
        Test.Database.Kosem.PostgreSQL.Internal.Sql.Typechecker.spec
    describe "Test.Database.Kosem.PostgreSQL.Schema.Internal.Parser" do
        Test.Database.Kosem.PostgreSQL.Schema.Internal.Parser.spec
    describe "Test.Database.Kosem.PostgreSQL.Internal.ToFromField" do
        Test.Database.Kosem.PostgreSQL.Internal.ToFromField.spec
    describe "Test.Database.Kosem.PostgreSQL.Internal.ToFromField (IO)" do
        Test.Database.Kosem.PostgreSQL.Internal.ToFromField.specIO
    describe "Test.Database.Kosem.PostgreSQL.Internal.Sql" do
        Test.Database.Kosem.PostgreSQL.Internal.Sql.spec
