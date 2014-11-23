{-# LANGUAGE OverloadedStrings #-}
module Network.Scraper.State where

import           Control.Applicative
import           Control.Arrow                    ((***))
import           Control.Lens                     ((^.))
import           Control.Monad
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Strict as ST
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust, fromMaybe, isJust,
                                                   listToMaybe)
import           Data.Monoid
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import qualified Data.Text.IO                     as TIO
import           Network.URL
import           Network.Wreq                     (FormParam (..))
import qualified Network.Wreq                     as Wreq
import           Network.Wreq.Session             (Session (..), withSession)
import qualified Network.Wreq.Session             as Sesh
import           Network.Wreq.Types
import           Safe
import           Text.HTML.DOM                    (parseLBS)
import           Text.XML.Cursor
import qualified Text.XML.Cursor.Generic          as CG

data ScraperState =
  PS { currentOptions :: Wreq.Options
     , currentHtml    :: LBS.ByteString
     , currentCursor  :: Maybe Cursor
     , currentSession :: Session
     , currentURL     :: Maybe URL
     } deriving (Show)

type Scraper = ST.StateT ScraperState IO

toCursor = fromDocument . parseLBS

withInitialState :: (ScraperState -> IO a) -> IO a
withInitialState callback = withSession $ \s -> do
  let initialState = PS { currentOptions = Wreq.defaults
                        , currentHtml = ("" :: LBS.ByteString)
                        , currentCursor = Nothing
                        , currentSession = s
                        , currentURL = Nothing
                        }
  callback initialState

runScraper :: Scraper a -> IO a
runScraper k = withInitialState (evalScraperWith k)

evalScraperWith :: Scraper a -> ScraperState -> IO a
evalScraperWith k s =  ST.evalStateT k s

-- TODO: Move somewhere else???
setCurrentOptions :: Wreq.Options -> Scraper ()
setCurrentOptions o = do
   scraper <- ST.get
   ST.put $ scraper { currentOptions = o }

setCurrentURL :: Maybe URL -> Scraper ()
setCurrentURL u = ST.get >>= (\s -> ST.put $ s { currentURL = u })

getCurrentURL :: Scraper(Maybe URL)
getCurrentURL = ST.get >>= return . currentURL

getCurrentHtml :: Scraper(LBS.ByteString)
getCurrentHtml = ST.get >>= return . currentHtml

-- TODO: Move somewhere else???
-- getCurrentPage :: Shpider Page
getCurrentCursor :: Scraper (Maybe Cursor)
getCurrentCursor = do
   scraper <- ST.get
   return $ currentCursor scraper

-- TODO: Move somewhere else???
getCurrentSession :: Scraper (Session)
getCurrentSession = do
   scraper <- ST.get
   return $ currentSession scraper

-- TODO: Move somewhere else???
setCurrentSession :: Session -> Scraper ()
setCurrentSession s = do
   scraper <- ST.get
   ST.put $ scraper { currentSession = s}

-- TODO: Move somewhere else???
setCurrentCursor :: Cursor -> Scraper ( )
setCurrentCursor c = do
   scraper <- ST.get
   ST.put $ scraper { currentCursor = Just c }

-- TODO: Move somewhere else???
setCurrentHtml :: LBS.ByteString -> Scraper ()
setCurrentHtml html = do
   scraper <- ST.get
   ST.put $ scraper { currentHtml = html }

-- TODO: Move somewhere else???
formShortInfo' f = formInfo'
  where
    go Nothing = "N/A"
    go (Just x) = x
    formInfo = (headMay . attribute "name" $ f, headMay . attribute "action" $ f)
    formInfo' = (\(x,y) -> (go x, go y)) formInfo

-- TODO: Move somewhere else...
ppTuple :: (T.Text, T.Text) -> T.Text
ppTuple = \(x,y) -> "[" <> x <> "]" <> ": " <> y

-- TODO: Move somewhere else...
printFormNames :: Scraper ()
printFormNames = do
  c <- getCurrentCursor
  let c' = fromMaybe (error "No cursor set") c
      forms = c' $// element "form"
      formInfo = map (ppTuple . formShortInfo') forms
  liftIO $ mapM_ (TIO.putStrLn) formInfo

-- TODO: Move somewhere else???
get :: String -> Scraper (LBS.ByteString)
get urlStr = do
  let url = fromMaybe (error ("invalid urlStr: " ++ urlStr)) (importURL urlStr)
      urlStr' = exportURL url
  -- TODO: Display under debug mode
  -- liftIO . putStrLn $ "GET: " ++ url ++ "\n"
  opts <- ST.gets currentOptions
  sesh <- ST.gets currentSession


  r <- liftIO $ Sesh.getWith opts sesh urlStr'
  let html = r ^. Wreq.responseBody
  setCurrentURL (Just url)
  setCurrentHtml html
  setCurrentCursor (toCursor html)
  return html

-- TODO: Move somewhere else???
post :: Postable a => String -> a -> Scraper (LBS.ByteString)
post urlStr params = do
  opts <- ST.gets currentOptions
  sesh <- ST.gets currentSession
  -- TODO: should take an actual url.. makes api more difficult...
  -- TODO: Make url absolute by storing host of current site
  -- TODO: Display under debug mode
  -- liftIO . print $ params
  -- TODO: Make minimal repro and ask question... not sure how to print something so polymorphic
  -- liftIO . putStrLn $ "POST: " ++ url ++ "\n"
  let url = fromMaybe (error ("invalid urlStr: " ++ urlStr)) (importURL urlStr)
  absURL <- toAbsUrl url
  let url' = exportURL absURL

  r <- liftIO $ Sesh.postWith opts sesh url' params
  let html = r ^. Wreq.responseBody
  setCurrentHtml html
  setCurrentCursor (toCursor html)
  return html

toAbsUrl :: URL -> Scraper(URL)
toAbsUrl u@(URL (Absolute _) _ _) = return u
toAbsUrl u@(URL _ _ _) = do
  hostUrl <- getCurrentURL
  let hostUrl' = fromMaybe (error errMsg) hostUrl
      absUrl = u { url_type = url_type hostUrl' }
  return absUrl
    where errMsg = "You must 'get' or 'post' to something before making urls absolute"
-- toAbsUrl u@(URL _ _ _) = return u

-- TODO: Move to tests
testToAbsUrl :: Scraper()
testToAbsUrl = do
  setCurrentURL (importURL "http://www.google.com")
  aUrl <- toAbsUrl (fromJust . importURL $ "blah.php")
  liftIO . print . exportURL $ aUrl

hasDisplayNone el = fromMaybe False . fmap (== "display: none;") . headMay $ (attribute "style" el)
hasHide el = fromMaybe False . fmap (T.isInfixOf "hide") . headMay $ (attribute "class" el)

anyParentIsHidden el = isJust . listToMaybe . join $ map (\c -> (c $/ check (hasHide))) cs
  where cs = ancestor el

-- checks to see if an eleemnt is displayed
-- isDisplayed el  = any (== True) $ [displayNone el, hide el]
isDisplayed :: Cursor -> Bool
isDisplayed el = all (== False) $
                 [ hasDisplayNone el
                 , hasHide el
                 , anyParentIsHidden el -- this was hiding inputs on the paypal billing form
                 ]

-- TODO: Move somewhere else???
 -- let aForm = toCursor "<form><input name=\"NOOO\" style=\"display: none;\"><input name=\"YES\"></form>"
getVisibleInputs :: Cursor -> M.Map T.Text T.Text
getVisibleInputs  c = do
  let inputs' = filter isDisplayed inputs
      mayPairs = map (\e -> (listToMaybe $ attribute "name" e, listToMaybe $ attribute "value" e)) inputs'
      pairs = map (fromMaybe "" *** fromMaybe "") mayPairs
  M.fromList $ filter ((/= "") . fst) pairs
  where inputs = c $// element "input"

getAllInputs :: Cursor -> M.Map T.Text T.Text
getAllInputs  c = do
  let mayPairs = map (\e -> (listToMaybe $ attribute "name" e, listToMaybe $ attribute "value" e)) inputs
      pairs = map (fromMaybe "" *** fromMaybe "") mayPairs
  M.fromList $ filter ((/= "") . fst) pairs
  where inputs = c $// element "input"

-- test get inputs
-- todo: Move to tests
tgi = do
  LBS.readFile "mismatchedinputkeyvalsform.html" >>= return . getAllInputs . toCursor

-- TODO: Move somewhere else???
getLoginForm url = get url >>= return . getAllInputs . toCursor

-- TODO: Move somewhere else???
toWreqFormParams :: [(T.Text, T.Text)] -> [FormParam]
toWreqFormParams params = map (\(k,v) -> k := v) (map (encodeUtf8 *** encodeUtf8) params)

-- TODO: Move somewhere else???
linkWithText :: T.Text -> Cursor -> Maybe Cursor
linkWithText t cursor = listToMaybe $ filter (\c -> (any (T.isInfixOf t)) (c $/ content)) (cursor $// element "a")

-- TODO: Move somewhere else???
addToMap pairs m = foldl (\m ->(\(k,v) -> M.insert k v m)) m pairs

-- TODO: Move somewhere else???
getFormByName :: T.Text -> Scraper (Maybe Cursor)
getFormByName name = do
  c <- getCurrentCursor
  let c' = fromMaybe (error "Nocursor set") c
      formList = c' $// element "form" >=> attributeIs "name" name
  return . listToMaybe  $ formList

-- TODO: Move somewhere else???
data FormAttr = Name T.Text | ActionUrl T.Text | FormId T.Text deriving Show

-- TODO: Move somewhere else???
getFormBy :: FormAttr -> Scraper (Maybe Cursor)
getFormBy formAttr = do
  c <- getCurrentCursor
  let c' = fromMaybe (error "Nocursor set") c
  return . listToMaybe  $ formList formAttr c'
    where formList (Name val) c =
            c $// element "form" >=> attributeIs "name" val
          formList (ActionUrl val) c =
            c $// element "form" >=> attributeIs "action" val
          formList (FormId val) c =
            c $// element "form" >=> attributeIs "id" val

getInputs True = getVisibleInputs
getInputs False = getAllInputs

fillForm :: Maybe Cursor -> Maybe [(T.Text, T.Text)] -> Bool -> [FormParam]
fillForm form Nothing filterHidden = do
  -- liftIO . putStrLn $ "old inputs: " ++ show (map (attribute "class") $ filter (not . isDisplayed) form)
  let formParams = fromMaybe (error "no params in form") (getInputs filterHidden <$> form)
  toWreqFormParams . M.toList $ formParams
fillForm form (Just params) filterHidden = do
  -- putStrLn $ "filling form: " ++ show form
  let formParams = fromMaybe (error "no params in form") (getInputs filterHidden <$> form)
      formParams' = addToMap params formParams
  toWreqFormParams . M.toList $ formParams'

-- TODO: Move somewhere else???
-- Takes a form name, fields to fill out in the form, then submits the form
-- TODO: Change all[ (T.Text,T.Text)] to just be wreq formvalues... neater api anyway
postToForm :: FormAttr -> Maybe [(T.Text,T.Text)] -> Bool -> Scraper (LBS.ByteString)
postToForm formAttr params filterHidden = do
  form <- getFormBy formAttr
  c <- getCurrentCursor
  case form of
   Just _ -> liftIO $ putStrLn $ "Found form: " ++ show formAttr
   Nothing -> do
     liftIO $ do
       putStrLn "forms found: "
       mapM_ TIO.putStrLn $ join $ (map (attribute "name") $ (fromJust c) $// element "form")
     error ("Couldn't find form: " ++ show formAttr)

  let formParams = fillForm form params filterHidden
      -- todo, this is duplicated above... delete one or the other if possible
      mActionUrl = T.strip <$> (join $ listToMaybe <$> attribute "action" <$> form)
      actionUrl = fromMaybe (error "Couldn't find action url in form") mActionUrl

  liftIO $ do
    TIO.putStrLn $ "POST " <> actionUrl
    print formParams

  getCurrentHtml >>= liftIO . LBS.writeFile "last.html"

  html <- post (T.unpack actionUrl) formParams
  return html
