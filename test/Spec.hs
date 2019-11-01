import Test.Hspec
import qualified Graph

main :: IO ()
main = hspec $ do
    describe "graph" Graph.spec
