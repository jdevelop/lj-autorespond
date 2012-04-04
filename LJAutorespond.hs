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

extractHTMLPart ::  Message -> [String]
extractHTMLPart = f . m_message_content 
  where
    f (Mixed (Multipart _ msgs _)) = concatMap extractHTMLPart msgs
    f (Alternative (Multipart _ msgs _)) = concatMap extractHTMLPart msgs
    f (Data _ (ContentType "text" "html" _) _ msg) = [msg]
    f _ = []

extractInputTags ::  String -> IO [(String, String)]
extractInputTags content = (filter ((/= "subject") . fst)) `fmap` (X.runX $
  X.readString [X.withValidate X.no, X.withParseHTML X.yes] content X.>>>
  XP.getXPathTrees "//form/input" X.>>>
  X.deep 
      ( X.isElem X.>>> (X.getAttrValue "name" X.&&& X.getAttrValue "value") ))

postMessage :: [(String,String)] -> IO ()
postMessage formData = do
  curl <- C.initialize
  C.withCurlDo $ do
    resp <- C.do_curl_ curl "http://www.livejournal.com/talkpost_do.bml" [
      C.CurlHttpHeaders ["Accept-Charset : utf-8"],
      C.CurlPost True,
      mkForm (("body","!!!"):formData)
      ] :: IO (C.CurlResponse)
    print $ C.respCurlCode resp
    return ()


mkForm :: [(String,String)] -> C.CurlOption
mkForm = C.CurlPostFields . map mkEncoded
  where
    mkEncoded (k,v) = f k ++ "=" ++ f v
    f = U.encode . US.encode
