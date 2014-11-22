import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit

trivialTest = testCase "mytest" $ assertEqual "" 1 1

tests = testGroup "All tests" [trivialTest]

main :: IO ()
main = defaultMain tests
