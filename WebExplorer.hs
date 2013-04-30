{-# LANGUAGE OverloadedStrings #-}
module WebExplorer where

import            Control.Lens.Lens
import            Control.Monad
import            Control.Monad.IO.Class
import            qualified Data.ByteString.Char8 as BS
import            Data.Maybe
import            qualified Data.Text as Text
import            System.Random
import            System.Directory
import            System.FilePath
import            Heist.Interpreted
import            Snap.Core
import            Snap.Http.Server
import            Snap.Snaplet
import            Snap.Snaplet.Heist.Interpreted
import            Deform hiding (main)

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
    explorer <- liftIO $ getExplorer fileName text "the"
    case explorer of
        Just e -> writeBS . BS.pack . content . document $ e
        Nothing -> writeBS "Error: No input"

getExplorer :: Maybe FilePath 
    -> Maybe BS.ByteString 
    -> String -> IO (Maybe Explorer)
getExplorer Nothing _ s = return Nothing
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
