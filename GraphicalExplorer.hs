module GraphicalExplorer where

import Deform
import Graphics.Gloss

main :: IO ()
main = display
    (InWindow
        "Deformed Explorer"
        (400, 500)
        (20, 20))
    black
    picture

picture :: Picture
picture = translate (-170) (-10)
    . scale 0.5 0.5
    . color white 
    . text $ "hello"
