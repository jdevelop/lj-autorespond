module LJAutorespond where

import qualified Text.XML.HXT.XPath as XP
import qualified Text.XML.HXT.Core as X

import qualified Network.Curl as C
import qualified Network.Curl.Code as CC
import qualified Network.Curl.Opts as CO

import Codec.MIME.String.Types
import Codec.MIME.String.Headers
import Codec.MIME.String.Parse

import Codec.Binary.Url as U                                                                                                            
import Codec.Binary.UTF8.String as US

import qualified System.IO.UTF8 as S8
import System.Random (randomIO)
import System.Environment
import System.Directory (getAppUserDataDirectory, doesFileExist)

import qualified Data.Array as A
import qualified Data.Text as T
import qualified Data.Set as DS
import qualified Data.Map as DM
import Data.List.Split (splitOn)

import Control.Monad (liftM)

type JournalMapping = DS.Set String
type UserMapping = DM.Map String JournalMapping
type InputTags = DM.Map String String

data AppConfig = AppConfig {
  userMapping :: UserMapping,
  fileConfig :: String
}  deriving (Show, Read)

appName = "lj-autorespond"

getConfigFileName = getAppUserDataDirectory appName

extractHTMLPart :: Message -> [String]
extractHTMLPart = f . m_message_content 
  where
    f (Mixed (Multipart _ msgs _)) = concatMap extractHTMLPart msgs
    f (Alternative (Multipart _ msgs _)) = concatMap extractHTMLPart msgs
    f (Data _ (ContentType "text" "html" _) _ msg) = [msg]
    f _ = []

extractUsername :: String -> IO String
extractUsername content = head `fmap` (X.runX $
  X.readString [X.withValidate X.no, X.withParseHTML X.yes] content X.>>>
  XP.getXPathTrees "//table/tr/td/a/text()" X.>>>
  X.deep 
    ( X.getText ))

extractInputTags :: String -> IO InputTags
extractInputTags content = (DM.fromList . filter ((/= "subject") . fst)) `fmap` (X.runX $
  X.readString [X.withValidate X.no, X.withParseHTML X.yes] content X.>>>
  XP.getXPathTrees "//form/input" X.>>>
  X.deep 
      ( X.isElem X.>>> (X.getAttrValue "name" X.&&& X.getAttrValue "value") ))

postMessage :: [(String,String)] -> IO ()
postMessage formData = do
  curl <- C.initialize
  C.withCurlDo $ do
    (C.do_curl_ curl $!) "http://www.livejournal.com/talkpost_do.bml" [
      C.CurlPost True,
      mkForm formData
      ] :: IO (C.CurlResponse)
    return ()


mkForm :: [(String,String)] -> C.CurlOption
mkForm = C.CurlPostFields . map mkEncoded
  where
    mkEncoded (k,v) = f k ++ "=" ++ f v
    f = U.encode . US.encode


getRandomPhrase :: String -> IO String
getRandomPhrase fileName = do
  linesArray <- readPhrases fileName
  lineNumber <- randomIO :: IO Int
  print lineNumber
  return $ linesArray A.! (lineNumber `mod` (succ (snd (A.bounds linesArray))))

readPhrases :: String -> IO (A.Array Int String)
readPhrases = liftM f . S8.readFile
  where
    f = g . filter ( (/= "")  ) . map unlines . splitOn [""] . map ( T.unpack . T.strip . T.pack ) . lines
    g lst = A.listArray (0, pred $ length lst) lst

processStdIn :: UserMapping -> String -> IO ()
processStdIn users replies = do
  content <- liftM (head . extractHTMLPart . parse) S8.getContents
  inputTags <- extractInputTags content
  username <- extractUsername content
  check inputTags username
  where
    check inputTags uname | not $ DM.member uname users = return ()
                          | otherwise = go inputTags (DM.lookup uname users)
    go inputTags Nothing = post inputTags
    go inputTags (Just journals) | DS.member (inputTags  DM.! "journal") journals = post inputTags
                                 | otherwise = return ()
    post inputTags = do
      body <- getRandomPhrase replies
      postMessage $ ("body",body) : (DM.toList $ DM.insert "encoding" "utf8" inputTags)


main = do
  fileNameFromDir <- getConfigFileName
  fileExists <- doesFileExist fileNameFromDir
  (AppConfig users replies) <- liftM read $ readFile fileNameFromDir
  if fileExists 
    then processStdIn users replies
    else fail $ "Can not read config file " ++ fileNameFromDir
