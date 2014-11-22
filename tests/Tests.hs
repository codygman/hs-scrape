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
  assertEqual "Parent hidden" False (isDisplayed parentHiddenInps)
    where dispNone = toCursor "<input name=\"t\" style=\"display: none;\">"
          classHidden = toCursor "<input name=\"t\" class=\"hide;\">"
          dispNoneClassHidden = toCursor "<input name=\"t\" class=\"hide;\">"
          visibleInp = toCursor "<input name=\"shown\">"
          parentHiddenInps = toCursor $ "\
          \<div class=\"domcheckertldselect hide\" id=\"tlds\">\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".ph\" checked=\"\" type=\"checkbox\"> .ph</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".com.ph\" type=\"checkbox\"> .com.ph</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".net.ph\" type=\"checkbox\"> .net.ph</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".org.ph\" type=\"checkbox\"> .org.ph</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".ngo.ph\" type=\"checkbox\"> .ngo.ph</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".mil.ph\" type=\"checkbox\"> .mil.ph</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".com\" type=\"checkbox\"> .com</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".net\" type=\"checkbox\"> .net</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".org\" type=\"checkbox\"> .org</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".biz\" type=\"checkbox\"> .biz</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".info\" type=\"checkbox\"> .info</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".name\" type=\"checkbox\"> .name</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".us\" type=\"checkbox\"> .us</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".ws\" type=\"checkbox\"> .ws</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".asia\" type=\"checkbox\"> .asia</label></div>\
                    \<div class=\"col4 textcenter\"><label class=\"full\"><input name=\"tlds[]\" value=\".tv\" type=\"checkbox\"> .tv</label></div>\
                \<div class=\"clear\"></div>\
    \</div>"

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
