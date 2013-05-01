module Deform   ( similars
                , move
                , document
                , docId
                , history
                , printHistory
                , content
                , initializeExplorer
                , replayHistory
                , newExplorer
                , loadExplorer
                , saveExplorer
                , indexFile
                , main
                , Explorer
                , Similars
                , Weight
                , Document
                , DocId
                ) where

import Index
import Control.Monad
import Data.List (intersperse)
import Data.Maybe
import System.Environment
import System.Directory

data Explorer = Explorer    { index :: Index 
                            , similars :: Similars 
                            , document :: Document
                            , history :: [DocId]
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
            explorer' <- explore .fromJust $ explorer
            writeFile "history" (printHistory explorer')

printHistory :: Explorer -> String
printHistory e = 
    let
        i = index e
        getTexts = intersperse "\n\n" . fmap (content . getDocument i)
    in
        concat . getTexts . history $ e

explore :: Explorer -> IO Explorer
explore e = do
    putStrLn . content . document $ e
    putStrLn ""
    let near = take 2 . similars $ e
    maybeNext <- selectPrint printScored near
    case maybeNext of
        Nothing -> putStrLn "That was fun" >> return e
        Just (_, next) -> 
            explore (move next e)

applyTransforms :: [(a -> a)] -> a -> a
applyTransforms [] a = a
applyTransforms (f:fs) a = a `seq` applyTransforms fs (f a)

replayHistory :: [DocId] -> Explorer -> Explorer
replayHistory ds e = 
    let
        i = index e
        moves = fmap (move . getDocument i) ds
    in
        applyTransforms moves e
            
move :: Document -> Explorer -> Explorer
move d e = 
    let
        i' = ignoreDocument d . index $ e
    in e    { index = i'
            , similars = computeSimilars i' d
            , history = (docId d:) . history $ e
            , document = d}

select :: Show a => [a] -> IO (Maybe a)
select = selectPrint show

selectPrint :: (a -> String) -> [a] -> IO (Maybe a)
selectPrint pp xs = do
    let labelledItems = zip [1..] xs
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
    let i = reads s :: [(Int, String)]
    in if null i then Nothing else Just . fst . head $ i

printScored :: (Weight, Document) -> String
printScored (w, d) = "[" ++ show w ++ "] " ++ content d

saveExplorer :: FilePath -> String -> IO ()
saveExplorer f = saveIndex f . buildIndex

loadExplorer :: FilePath -> String -> IO (Maybe Explorer)
loadExplorer f s = do
    goodf <- doesFileExist f
    if goodf then do
        i <- loadIndex f
        case i of
            Left _ -> return Nothing
            Right i' -> return $ initializeExplorer i' s
    else return Nothing

newExplorer :: FilePath -> String -> IO (Maybe Explorer)
newExplorer f s = do
    goodf <- doesFileExist f
    if goodf then do
        i <- indexFile f
        return $ initializeExplorer i s
    else return Nothing

initializeExplorer :: Index -> String -> Maybe Explorer
initializeExplorer i s = 
    let
        start = searchIndex i s
        d = if null start then Nothing else Just . snd . head $ start
    in do
        d' <- d
        i' <- return $ (ignoreDocument d') i
        return $ Explorer i' (take 20 $ computeSimilars i' d') d' []
