{-# LANGUAGE OverloadedStrings #-}
module WebExplorer where

import Control.Lens.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace
import System.Random
import System.Directory
import System.FilePath
import Heist.Interpreted
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Snaplet
import Snap.Snaplet.Heist.Interpreted
import Deform hiding (main)

type SnapApp = Snaplet (Heist App)

data App = App { _heist :: SnapApp }

heist :: Functor f => (SnapApp -> f SnapApp) -> App -> f App 
heist = lens _heist (\a s -> a { _heist = s})

instance HasHeist App where 
    heistLens = subSnaplet heist

appInit :: SnapletInit App App
appInit = makeSnaplet "app" "" Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    root <- liftM (Text.pack . BS.unpack) getSnapletRootURL
    modifyHeistState (bindStrings [("appRoot", root)])
    addRoutes   [ ("deform", entryHandler)
                , ("deform/:deformedId", deformedHandler)
                , ("deform/:deformedId/explorer", viewerHandler)
                ]
    return $ App h

main :: IO ()
main = serveSnaplet productionConfig appInit

productionConfig :: MonadSnap m => Config m a
productionConfig    = setBind "nullcanvas.com" 
                    . setPort 4000
                    $ defaultConfig

debug :: IO ()
debug = serveSnaplet defaultConfig appInit

viewerHandler :: Handler App App ()
viewerHandler = do
    deformedId <- getParam "deformedId"
    seed <- getParam "seed"
    let seed' = maybe "the" (Text.pack . BS.unpack) seed
    case deformedId of
        Nothing -> errorPage
        Just i -> bind    [("deformedId", Text.pack . BS.unpack $ i)
                          ,("seed", seed')
                          ] $ render "viewer"

errorPage :: Handler App App ()
errorPage = writeBS "Error"

deformedHandler :: Handler App App ()
deformedHandler = do
    modifyResponse (setContentType "text/javascript")
    redir <- getParam "redir"
    deformedId <- getParam "deformedId"
    let fileName = fmap BS.unpack deformedId
    text <- getParam "text"
    hist <- getParam "history"
    seed <- getParam "seed"
    let seed' = maybe "the" BS.unpack seed
    explorer <- liftIO $ getExplorer fileName text seed'
    let toSeeded k = BS.concat [k, "?seed=", BS.pack seed']
    when (isJust redir) $ redirect (toSeeded . fromJust $ redir)
    case explorer of
        Just e -> do
            let explorer' = replay hist e
            writeLBS . encodeExplorer $ explorer'
        Nothing -> writeBS "{}"

encodeExplorer :: Explorer -> LBS.ByteString
encodeExplorer e =
    let
        sims = take 5 . similars $ e
        mapSim (w, d) = toJSON $ Map.fromList 
            ([ ("weight", toJSON w)
            , ("content", toJSON $ content d)
            , ("id", toJSON . show . docId $ d)
            ] :: [(Text, Value)])
        mappedSims = toJSON . fmap mapSim $ sims
        doc = toJSON . content . document $ e
        h = toJSON . history $ e
        explorer = toJSON $ Map.fromList
            ([ ("similars", mappedSims)
            , ("document", doc)
            , ("history", h)
            ] :: [(Text, Value)])
    in
        encode explorer

replay :: Maybe BS.ByteString -> Explorer -> Explorer
replay Nothing = id
replay (Just h) = 
    let 
        hist = BS.unpack h
        hs = fmap fst . concat . fmap reads . splitOn ":" $ hist
    in replayHistory hs

getExplorer :: Maybe FilePath 
    -> Maybe BS.ByteString 
    -> String -> IO (Maybe Explorer)
getExplorer Nothing _ _ = return Nothing
getExplorer (Just f) Nothing s = do
    createDirectoryIfMissing True "texts"
    let fileName = "texts" </> f
    loadExplorer fileName s 
getExplorer (Just f) (Just t) s = do
    createDirectoryIfMissing True "texts"
    let fileName = "texts" </> f
    me <- loadExplorer fileName s
    case me of
        Nothing -> do 
            let maxSize = 1000 * 1000 * 5
            saveExplorer fileName (BS.unpack . BS.take maxSize $ t)
            loadExplorer fileName s
        e -> return e

entryHandler :: Handler App App ()
entryHandler = do
    hash <- liftIO $ liftM computeHash newStdGen
    bind [("deformedId", Text.pack hash)] $ render "deform"

computeHash :: RandomGen g => g -> String
computeHash = 
    let
        chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
        l = length chars - 1
    in take 9 . fmap (chars !!) . randomRs (0, l)

type Text = Text.Text

bind :: HasHeist b => [(Text, Text)] -> Handler b v a -> Handler b v a
bind ss = heistLocal (bindStrings ss)
