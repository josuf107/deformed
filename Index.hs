module Index    ( content
                , buildIndex
                , indexFile
                , incrementIndex
                , ignoreDocument
                , getDocument
                , docId
                , searchIndex
                , computeSimilars
                , Index
                , Document
                , DocId
                , Similars
                , Weight
                ) where

import Control.Monad
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Debug.Trace
import Text.ParserCombinators.Parsec

type Word = String
type Content = String
type Vector = Map.Map Word Weight
data Document = Document    { content :: Content
                            , vectors :: Vector
                            , tfIdf :: Vector
                            , docId :: DocId
                            , maxFreq :: Weight
                            , visible :: Bool
                            } deriving Show
type Scores = Map.Map Weight [Document]
type Similars = [(Weight, Document)]
type Weight = Float
type InvertedIndex = Map.Map Word [DocId]
type Dfs = Map.Map Word Int
type DocId = Int
data DocStore = DocStore    { docStore :: Map.Map DocId Document
                            , nextId :: DocId
                            } deriving Show
data Index = Index  { docs :: DocStore
                    , dfs :: Dfs
                    , index :: InvertedIndex
                    } deriving Show

applyTransforms :: [(a -> a)] -> a -> a
applyTransforms [] a = a
applyTransforms (f:fs) a = a `seq` applyTransforms fs (f a)

searchIndex :: Index -> Content -> Similars
searchIndex i c = 
    let
        i' = incrementIndex c i
        thisId = nextId . docs $ i
        d' = (Map.! thisId) . docStore . docs $ i'
    in computeSimilars i' d'

computeSimilars :: Index -> Document -> Similars
computeSimilars i = nearestSims . computeSims i

nearestSims :: Scores -> Similars
nearestSims = 
    concat . fmap (\(w, ds) -> zip (repeat w) ds) . Map.toDescList

computeSims :: Index -> Document -> Scores
computeSims i d = 
    let
        ds = Map.elems . Map.delete (docId d) . docStore . docs $ i
        ds' = filter visible ds
        scores = fmap (\d2 -> (computeSim d d2, [d2])) ds'
    in foldl' (flip . uncurry $ Map.insertWith (++)) Map.empty scores

computeSim :: Document -> Document -> Weight
computeSim d1 d2 =
    let
        tfIdf1 = tfIdf d1
        tfIdf2 = tfIdf d2
        l = vectorLength tfIdf1 * vectorLength tfIdf2
        keys1 = Map.keysSet tfIdf1
        keys2 = Map.keysSet tfIdf2
        keys = Set.toList $ keys1 `Set.intersection` keys2
        tf k = (tfIdf1 Map.! k) * (tfIdf2 Map.! k) / l
        scores = fmap tf  keys
    in  sum scores

vectorLength :: Vector -> Weight
vectorLength = sqrt . sum . fmap (^(2::Integer)) . Map.elems
        
emptyDocStore :: DocStore
emptyDocStore = DocStore { docStore = Map.empty, nextId = 0 }

emptyIndex :: Index
emptyIndex = Index  { docs = emptyDocStore 
                    , dfs = Map.empty
                    , index = Map.empty
                    }

indexFile :: FilePath -> IO Index
indexFile = liftM buildIndex . readFile

incrementIndex :: Content -> Index -> Index
incrementIndex c = computeTfIdf . addDocument (initializeDocument c)

getDocument :: Index -> DocId -> Document
getDocument i d = (Map.! d) . docStore . docs $ i 

ignoreDocument :: Document -> Index -> Index
ignoreDocument d i = 
    let
        ignoreId = docId d
        ignore d = d { visible = False }
        docStore' = (Map.adjust ignore ignoreId) . docStore . docs $ i
    in
        i { docs = (docs i) {docStore = docStore'} }

buildIndex :: String -> Index
buildIndex s =
    let
        documents = getDocuments s
        addDocuments = fmap addDocument documents
        unscored = applyTransforms addDocuments emptyIndex
    in
        computeTfIdf unscored

computeTfIdf :: Index -> Index
computeTfIdf i@(Index { docs = ds, dfs = idfs }) = 
    let
        tfs d = Map.map (/ maxFreq d) . vectors $ d
        ndocs = fromIntegral $ nextId ds - 1 :: Float
        idfs' = Map.map (\d -> log $ ndocs / fromIntegral d) idfs
        idf k a = (idfs' Map.! k) * a
        tfIdfs d = d { tfIdf = Map.mapWithKey idf . tfs $ d }
        ds' = ds { docStore = Map.map tfIdfs . docStore $ ds}
    in
        i { docs = ds' }

addDocument :: Document -> Index -> Index
addDocument d i = 
    let
        vd = tfDocument d
        n = nextId . docs $ i
    in
        Index   { docs = addToDocStore vd (docs i)
                , dfs = computeDfs vd (dfs i)
                , index = addIndex (vd {docId = n}) (index i)
                }

addToDocStore :: Document -> DocStore -> DocStore
addToDocStore d (DocStore { docStore = ds, nextId = n }) = 
    let 
        d' = d { docId = n } 
    in
        DocStore { docStore = Map.insert n d' ds, nextId = n + 1}

addIndex :: Document -> InvertedIndex -> InvertedIndex
addIndex d = 
    let indexWords = fmap indexWord . Map.keys . vectors $ d
    in applyTransforms indexWords
    where
        indexWord :: Word -> InvertedIndex -> InvertedIndex
        indexWord w m
            | w `Map.member` m = Map.adjust (docId d :) w m
            | otherwise = Map.insert w [docId d] m

computeDfs :: Document -> Dfs -> Dfs
computeDfs documents = 
    let countWords = fmap countWord . Map.keys . vectors $ documents
    in applyTransforms countWords
    where
        countWord :: Word -> Dfs -> Dfs
        countWord w m
            | w `Map.member` m = Map.adjust (+1) w m
            | otherwise = Map.insert w 1 m

initializeDocument :: Content -> Document
initializeDocument s = Document s Map.empty Map.empty 0 0 True

tfDocument :: Document -> Document
tfDocument document = 
    let addWords = fmap addWord . getWords . content $ document
    in applyTransforms addWords document

addWord :: Word -> Document -> Document
addWord w d@(Document {vectors = vs, maxFreq = freq})
    | w `Map.member` vs = d { vectors = Map.adjust (+1) w vs
                            , maxFreq = max freq (vs Map.! w + 1)
                            }
    | otherwise = d { vectors = Map.insert w 1 vs
                    , maxFreq = max freq 1
                    }

getWords :: Content -> [Word]
getWords c = 
    let 
        start = unwords . take 4 . words $ c
        result = parse contentParser start c
    in
        case result of
            Right success -> success
            Left err -> error . show $ err

contentParser :: GenParser Char st [Word]
contentParser = do
    wordSeparator
    sepEndBy wordParser wordSeparator
        where
            wordParser = do
                word <- many1 letter
                return . normalize $ word
            separatorEnd = lookAhead (void letter <|> eof)
            wordSeparator = void $ anyChar `manyTill` separatorEnd

getDocuments :: String -> [Document]
getDocuments s =
    let 
        start = unwords . take 4 . words $ s
        result = parse sourceParser start s
    in
        case result of
            Right success -> fmap initializeDocument success
            Left err -> error . show $ err

sourceParser :: GenParser Char st [Content]
sourceParser = 
    sepEndBy paragraphParser paragraphSeparator
        where
            paragraphParser = do
                anyChar `manyTill` lookAhead letter
                anyChar `manyTill` paragraphEnd
            paragraphEnd = try . lookAhead $ paragraphSeparator
            eol = void $ try (string "\n\r")
                    <|> try (string "\r\n")
                    <|> string "\n"
                    <|> string "\r"
            paragraphSeparator = (eol >> eol) <|> eof

normalize :: String -> Word
normalize = fmap toLower
