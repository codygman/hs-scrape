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
import           Data.Maybe                       (fromJust, fromMaybe,
                                                   listToMaybe)
import           Data.Monoid
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import qualified Data.Text.IO                     as TIO
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
     } deriving (Show)

type Scraper = ST.StateT ScraperState IO

toCursor = fromDocument . parseLBS

withInitialState :: (ScraperState -> IO a) -> IO a
withInitialState callback = withSession $ \s -> do
  let initialState = PS { currentOptions = Wreq.defaults
                        , currentHtml = ("" :: LBS.ByteString)
                        , currentCursor = Nothing
                        , currentSession = s
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
get url = do
  liftIO . putStrLn $ "GET: " ++ url ++ "\n"
  opts <- ST.gets currentOptions
  sesh <- ST.gets currentSession
  r <- liftIO $ Sesh.getWith opts sesh url
  let html = r ^. Wreq.responseBody
  setCurrentHtml html
  setCurrentCursor (toCursor html)
  return html

-- TODO: Move somewhere else???
post :: Postable a => String -> a -> Scraper (LBS.ByteString)
post url params = do
  liftIO . putStrLn $ "POST: " ++ url ++ "\n"
  opts <- ST.gets currentOptions
  sesh <- ST.gets currentSession
  -- liftIO . print $ params -- TODO: Make minimal repro and ask question... not sure how to print something so polymorphic
  r <- liftIO $ Sesh.postWith opts sesh url params
  let html = r ^. Wreq.responseBody
  setCurrentHtml html
  setCurrentCursor (toCursor html)
  return html

-- TODO: Move somewhere else???
getInputs :: Cursor -> M.Map T.Text T.Text
getInputs c = do
  let elements = c $// element "input"
      mayPairs = map (\e -> (listToMaybe $ attribute "name" e, listToMaybe $ attribute "value" e)) elements
      pairs = map (fromMaybe "" *** fromMaybe "") mayPairs
  M.fromList $ filter ((/= "") . fst) pairs

-- test get inputs
-- todo: Move to tests
tgi = do
  LBS.readFile "mismatchedinputkeyvalsform.html" >>= return . getInputs . toCursor

-- TODO: Move somewhere else???
getLoginForm url = get url >>= return . getInputs . toCursor

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
data FormAttr = Name T.Text | ActionUrl T.Text

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

-- TODO: Move somewhere else???
-- Takes a form name, fields to fill out in the form, then submits the form
-- TODO: Change all[ (T.Text,T.Text)] to just be wreq formvalues... neater api anyway
postToForm :: FormAttr -> [(T.Text,T.Text)] -> Scraper (LBS.ByteString)
postToForm formAttr params = do
  form <- getFormBy formAttr
  liftIO . print $ "got form by name"
  let formParams = fromMaybe (error "no params in form") (getInputs <$> form)
      formParams' = addToMap params formParams
      actionUrl = fromMaybe (error "Couldn't find action url in form") $
                  T.strip <$> (join $ listToMaybe <$> attribute "action" <$> form)
  liftIO . print $ "posting params: \n" ++ show formParams'
  html <- post (T.unpack actionUrl) (toWreqFormParams . M.toList $ formParams')
  liftIO . print $ "done posting to form"
  return html
