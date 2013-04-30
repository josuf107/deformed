module GraphicalExplorer where

import Deform
import Graphics.UI.GLUT as G

main :: IO ()
main = do
    (_, _) <- getArgsAndInitialize
    createWindow "Deformed Explorer"
    displayCallback $= display
    mainLoop

display :: IO ()
display = do
    clear [ G.ColorBuffer ]
    i <- indexFile "middlemarch"
    let e = initializeExplorer i "the"
    scale 0.0002 0.0002 (0.0002 :: GLfloat)
    let s = content . document $ e
    sequence_ . fmap (uncurry displayString) $ zip (repeat s) stringVertices
    flush

stringVertices :: [Vertex3 GLfloat]
stringVertices = fmap (\n -> vx3 n n 0) . take 200 $ [-1, -1.01..]

displayString :: String -> Vertex3 GLfloat -> IO ()
displayString s v = do
    vertex v
    renderString Roman s

v3 :: GLfloat -> GLfloat -> GLfloat -> Vector3 GLfloat
v3 = Vector3

vx3 :: GLfloat -> GLfloat -> GLfloat -> Vertex3 GLfloat
vx3 = Vertex3
