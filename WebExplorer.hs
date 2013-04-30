{-# LANGUAGE OverloadedStrings #-}
module WebExplorer where

import            Control.Lens.Lens
import            Control.Monad
import            Control.Monad.IO.Class
import            qualified Data.Text as Text
import            System.Random
import            Heist.Interpreted
import            Snap.Core
import            Snap.Http.Server
import            Snap.Snaplet
import            Snap.Snaplet.Heist.Interpreted
import            Deform

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
    text <- getParam "text"
    maybe (writeBS "need text") writeBS text
    writeBS " "
    maybe (writeBS "Need id") writeBS deformedId

entryHandler :: Handler App App ()
entryHandler = do
    g <- liftIO newStdGen
    let hash = computeHash g
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
