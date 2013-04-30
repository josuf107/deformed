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
import qualified Data.Text as Text
import System.Random
import System.Directory
import System.FilePath
import Heist.Interpreted
import Snap.Core
import Snap.Http.Server
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
    modifyHeistState (bindStrings [("appRoot", "http://0.0.0.0:8000")])
    addRoutes   [ ("deform", entryHandler)
                , ("deform/:deformedId", deformedHandler)
                ]
    return $ App h

main :: IO ()
main = serveSnaplet defaultConfig appInit

deformedHandler :: Handler App App ()
deformedHandler = do
    deformedId <- getParam "deformedId"
    let fileName = fmap BS.unpack deformedId
    text <- getParam "text"
    hist <- getParam "history"
    explorer <- liftIO $ getExplorer fileName text "the"
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
    newExplorer fileName s 
getExplorer (Just f) (Just t) s = do
    createDirectoryIfMissing True "texts"
    let fileName = "texts" </> f
    me <- newExplorer fileName s
    case me of
        Nothing -> do 
            BS.writeFile fileName t
            newExplorer fileName s
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
