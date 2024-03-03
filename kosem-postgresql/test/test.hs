import Control.Exception (evaluate)
import Test.Database.Kosem.PostgreSQL.Internal qualified
import Test.Database.Kosem.PostgreSQL.Internal.Parser qualified
import Test.Database.Kosem.PostgreSQL.Schema.Internal.Parser qualified
import Test.Database.Kosem.PostgreSQL.Internal.ToFromField qualified
import Test.Hspec

main :: IO ()
main = hspec spec

spec = do
  describe "Test.Database.Kosem.PostgreSQL.Internal" do
    Test.Database.Kosem.PostgreSQL.Internal.spec
  describe "Test.Database.Kosem.PostgreSQL.Internal.Parser" do
    Test.Database.Kosem.PostgreSQL.Internal.Parser.spec
  describe "Test.Database.Kosem.PostgreSQL.Schema.Internal.Parser" do
    Test.Database.Kosem.PostgreSQL.Schema.Internal.Parser.spec
  describe "Test.Database.Kosem.PostgreSQL.Internal.ToFromField" do
    Test.Database.Kosem.PostgreSQL.Internal.ToFromField.spec
  describe "Test.Database.Kosem.PostgreSQL.Internal.ToFromField (IO)" do
    Test.Database.Kosem.PostgreSQL.Internal.ToFromField.specIO
