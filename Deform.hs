module Deform   ( similars
                , move
                , document
                , history
                , printHistory
                , content
                , initializeExplorer
                , indexFile
                , Explorer
                , Similars
                , Document
                ) where

import Index
import qualified Data.Map as Map
import Data.List (intersperse)
import System.Environment
import System.Directory

data Explorer = Explorer    { index :: Index 
                            , similars :: Similars 
                            , document :: Document
                            , history :: [Document]
                            }

main :: IO ()
main = do
    fs <- getArgs
    if null fs then putStrLn "No input file"
    else do
        let f = head fs
        goodf <- doesFileExist f
        if not goodf then putStrLn "Input file does not exist"
        else do
            i <- indexFile f
            let explorer = initializeExplorer i "the"
            explorer' <- explore explorer
            writeFile "history" (printHistory explorer')

printHistory :: Explorer -> String
printHistory = concat . intersperse "\n\n" . fmap content . history

explore :: Explorer -> IO Explorer
explore e = do
    putStrLn . content . document $ e
    putStrLn ""
    let near = take 2 . similars $ e
    maybeNext <- selectPrint printScored near
    case maybeNext of
        Nothing -> putStrLn "That was fun" >> return e
        Just (_, next) -> 
            explore (move e next)
            
move :: Explorer -> Document -> Explorer
move e d = 
    let
        i' = ignoreDocument d . index $ e
    in e    { index = i'
            , similars = computeSimilars i' d
            , history = (d:) . history $ e
            , document = d}

select :: Show a => [a] -> IO (Maybe a)
select = selectPrint show

selectPrint :: (a -> String) -> [a] -> IO (Maybe a)
selectPrint pp xs = do
    let labelledItems = zip [1..] $ xs
    let displayItem (n,s) = (n, show n ++ "> " ++ pp s)
    let displayItems = fmap displayItem labelledItems
    sequence_ . fmap (putStrLn . snd) $ displayItems
    putStrLn "Enter selection"
    n <- getLine
    if n == "q" then return Nothing
    else do
        let mi = readInt n
        case mi of
            Nothing -> selectPrint pp xs
            Just i ->
                case lookup i labelledItems of
                    Nothing -> selectPrint pp xs
                    Just a -> return . Just $ a

readInt :: String -> Maybe Int
readInt s = 
    let i = (reads s) :: [(Int, String)]
    in if null i then Nothing else Just . fst . head $ i

printScored :: (Weight, Document) -> String
printScored (w, d) = "[" ++ show w ++ "] " ++ content d

initializeExplorer :: Index -> String -> Explorer
initializeExplorer i s = 
    let
        d = snd . head . searchIndex i $ s
    in Explorer i (take 20 $ computeSimilars i d) d []
