{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map              as M
import           Network.Scraper.State
import           Test.Tasty            (defaultMain, testGroup)
import           Test.Tasty.HUnit
import           Text.XML.Cursor

trivialTest = testCase "mytest" $ assertEqual "" 1 1

testDisplayNone = testCase "testDisplayNone" $ do
  assertEqual "" True (hasDisplayNone inp)
  where inp = toCursor "<input name=\"t\" style=\"display: none;\">"

testClassHide = testCase "testClassHide" $ do
  assertEqual "" True (hasHide inp)
  where inp = toCursor "<input name=\"t\" class=\"hide;\">"

testIsDisplayedAll = testCase "testIsDisplayedAll" $ do
  assertEqual "Not Displayed (has display: none;)"  False (isDisplayed dispNone)
  assertEqual "Not Displayed (has class: hide)" False (isDisplayed classHidden)
  assertEqual "Not Displayed (has hide and dispNone)"  False (isDisplayed dispNoneClassHidden)
  assertEqual "Is Displayed" True (isDisplayed visibleInp)
    where dispNone = toCursor "<input name=\"t\" style=\"display: none;\">"
          classHidden = toCursor "<input name=\"t\" class=\"hide;\">"
          dispNoneClassHidden = toCursor "<input name=\"t\" class=\"hide;\">"
          visibleInp = toCursor "<input name=\"shown\">"

testGetInputs = testCase "testGetInputs" $ do
  assertEqual "" (getInputs form) (M.fromList [("YES","")])
  where form = toCursor "<form><input name=\"NOOO\" style=\"display: none;\"><input name=\"YES\"></form>"

tests = testGroup "All tests" [ testDisplayNone
                              , testClassHide
                              , testIsDisplayedAll
                              , testGetInputs
                              ]

main :: IO ()
main = defaultMain tests
