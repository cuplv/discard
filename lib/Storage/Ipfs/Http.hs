{-# LANGUAGE OverloadedStrings #-}

-- | IPFS bindings via the go-ipfs daemon's HTTP API
module Storage.Ipfs.Http 
  ( IpfsHttp
  , mkIpfsHttp
  , setDebugLevel
  , put
  , get
  , refs
  ) where

import qualified Data.Text as Text
import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Aeson
import Data.Aeson.Parser (decodeWith)
import Data.Aeson.Types (parse)
import Control.Monad
import System.Exit (die)
import System.IO (hFlush,stdout)

import Storage.Ipfs.Types

parse' p b = decodeWith json (parse p) b

data IpfsHttp = IpfsHttp 
  { ipfsHttpManager :: Manager
  , ipfsHttpApi :: String
  , ipfsHttpDebug :: Int }

-- | Create an IPFS HTTP API endpoint connection from a URI 'String'
-- (e.g. "http://localhost:5001")
mkIpfsHttp :: String
           -> IO IpfsHttp
mkIpfsHttp addr = IpfsHttp
  <$> newManager defaultManagerSettings
  <*> pure addr
  <*> pure 0

-- | Set how much information about each HTTP request is printed to
-- the console.  Default level is @0@, meaning print nothing.  Level
-- @1@ announces the start and end of each request, and level @2@
-- additionally prints the contents of requests.
setDebugLevel :: Int -> IpfsHttp -> IpfsHttp
setDebugLevel dbl (IpfsHttp m a _) = IpfsHttp m a dbl

dbg :: Int -> Int -> String -> IO ()
dbg t d s = if d >= t
               then putStrLn s >> hFlush stdout
               else return ()

dbgH = dbg 2

dbgL = dbg 1

putRespP = withObject "PutResponse" $ \v -> do
  hash <- v .: "Hash"
  return (IpfsPath hash)

-- | Put an IPFS object, returning the ref created for it
--
-- Example:
-- 
-- > putBaz :: IO IpfsPath
-- > putBaz = do api <- mkIpfsHttp "localhost" "5001"
-- >             foo <- put api (objData "foo")
-- >             bar <- put api (objData "bar")
-- >             put api (objData "baz" <> objLink "l1" foo <> objLink "l2" bar)
put :: IpfsHttp -> IpfsObject -> IO IpfsPath
put (IpfsHttp man host dbl) obj = do
  initReq <- parseRequest ("POST " ++ host ++ "/api/v0/object/put")
  fullReq <- formDataBody [partLBS "file" (encode obj)] initReq
  dbgL dbl "Starting IPFS HTTP put..."
  dbgH dbl (show obj)
  Just path <- (parse' putRespP) . responseBody <$> httpLbs fullReq man
  dbgH dbl (show path)
  dbgL dbl "Finished IPFS HTTP put."
  return path

-- | Get an IPFS object
get :: IpfsHttp -> IpfsPath -> IO IpfsObject
get (IpfsHttp man host dbl) (IpfsPath hash) = do
  fullReq <- parseRequest 
               ("POST " ++ host ++ "/api/v0/object/get?arg=" ++ Text.unpack hash)
  dbgL dbl "Starting IPFS HTTP get..."
  dbgH dbl (show hash)
  resp <- httpLbs fullReq man :: IO (Response BL.ByteString)
  case statusCode . responseStatus $ resp of
    200 -> return ()
    c -> CL.putStrLn (responseBody resp)
         >> die ("Error: Code " ++ show c ++ " from IPFS")
  obj <- case decode (responseBody resp) of
           Just obj -> return obj
           Nothing -> CL.putStrLn (responseBody resp)
                      >> die "Error: Could not decode IPFS response."
  dbgH dbl (show obj)
  dbgL dbl "Finished IPFS HTTP get."
  return obj

putBaz :: IpfsHttp -> IO IpfsPath
putBaz api = do foo <- put api (objData "foo")
                bar <- put api (objData "bar")
                put api (objData "baz" <> objLink "l1" foo <> objLink "l2" bar)

testy = do api <- mkIpfsHttp "http://localhost:5001"
           baz <- putBaz api
           quux <- put api (objData "quux" <> objLink "baznode" baz)
           Prelude.putStrLn "Refs of baz:"
           mapM_ print =<< refs api False baz
           Prelude.putStrLn "R-refs of baz:"
           mapM_ print =<< refs api True baz
           Prelude.putStrLn "Refs of quux:"
           mapM_ print =<< refs api False quux
           Prelude.putStrLn "R-Refs of quux:"
           mapM_ print =<< refs api True quux

refsRespP = withObject "RefsResponse" $ \v -> do
  hash <- v .: "Ref"
  return (IpfsPath hash)

-- | List the links (direct or recursive) from the object pointed
-- to by the supplied ref
refs :: IpfsHttp 
     -> Bool -- ^ Recursive?
     -> IpfsPath -- ^ Root
     -> IO [IpfsPath]
refs (IpfsHttp man host dbl) recursive (IpfsPath hash) = do
  let rb = if recursive
              then "true"
              else "false"
  fullReq <- parseRequest ("POST " 
                           ++ host 
                           ++ "/api/v0/refs?arg=" 
                           ++ Text.unpack hash 
                           ++ "&recursive=" 
                           ++ rb)
  dbgL dbl "Starting IPFS HTTP refs..."
  dbgH dbl (show hash)
  rs <- map BL.fromStrict 
        . C.lines 
        . BL.toStrict 
        . responseBody 
        <$> httpLbs fullReq man
  dbgL dbl "Finished IPFS HTTP refs."
  let p vs b = case (parse' refsRespP b) of
                 Just v -> Just (v:vs)
                 Nothing -> Nothing
  case foldM p [] rs of
    Just vs -> return vs
