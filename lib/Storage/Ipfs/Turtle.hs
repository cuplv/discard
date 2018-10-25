{-# LANGUAGE OverloadedStrings #-}

module Storage.Ipfs.Turtle
  ( IpfsM
  , IpfsME
  , withDef
  , withApi
  , withApi'
  , withApiM
  , hdie
  , l0
  , l1
  , l2
  , IpfsApi
  , mkIpfsApi
  , defIpfsApi
  , ipfs
  , ipfst
  , addf
  , adds
  , get
  , Storage.Ipfs.Turtle.cat
  , pin
  , getObject
  , putObject

  -- * Helpful re-exports
  , throwError
  , liftEither

  ) where

import System.IO
import Data.Yaml
import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as Text
import Turtle hiding (cat,Parser,FilePath)
import qualified Turtle
import qualified Turtle.Bytes as TB
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict,fromStrict)
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import Data.Map (Map)
import qualified Data.Map as Map

import Storage.Ipfs.Types

withApi :: IpfsApi -> IpfsME a -> IO (Either Text a)
withApi api m = runReaderT (runExceptT m) api

withApi' :: Text -> IpfsME a -> IO (Either Text a)
withApi' t m = do eapi <- mkIpfsApi t
                  case eapi of
                    Right api -> runReaderT (runExceptT m) api
                    Left e -> return $ Left e

withApiM :: Maybe Text -> IpfsME a -> IO (Either Text a)
withApiM mt = case mt of
                Just t -> withApi' t
                Nothing -> withDef

withDef :: IpfsME a -> IO (Either Text a)
withDef m = do eapi <- defIpfsApi
               case eapi of
                 Right api -> runReaderT (runExceptT m) api
                 Left e -> return $ Left e

hdie :: (MonadIO m) => m (Either Text a) -> m a
hdie m = do r <- m
            case r of
              Right a -> return a
              Left e -> liftIO$ die e

-- | Create an API from a 'Text' URI, checking that it is reachable
mkIpfsApi :: Text -> IO (Either Text IpfsApi)
mkIpfsApi api = do 
  (c,_,err) <- procStrictWithErr "ipfs" ["--api",api,"id"] empty
  return (case c of
            ExitFailure _ -> Left err
            ExitSuccess -> Right (IpfsApi api))

-- | Create a handle for the default API, checking that it is reachable
defIpfsApi :: IO (Either Text IpfsApi)
defIpfsApi = do (c,_,err) <- procStrictWithErr "ipfs" ["id"] empty
                return (case c of
                          ExitFailure _ -> Left err
                          ExitSuccess -> Right DefIpfsApi)


lIpfsPath :: IpfsPath -> Line
lIpfsPath = unsafeTextToLine . fIpfsPath

fipp = fIpfsPath
fippFP = fromText . fipp


ipfs :: [Text] -> Shell ByteString -> IpfsME ByteString
ipfs args input = do 
  api <- l1$ ask
  (c,r,l) <- l2$ case api of
    IpfsApi t -> TB.procStrictWithErr "ipfs" (["--api",t] ++ args) input
    DefIpfsApi -> TB.procStrictWithErr "ipfs" args input
  (case c of
     ExitFailure _ -> throwError (decodeUtf8 l)
     ExitSuccess -> return r)

ipfst :: [Text] -> Shell ByteString -> IpfsME Text
ipfst args input = decodeUtf8 <$> ipfs args input

-- | Add a file
addf :: Turtle.FilePath -> IpfsME IpfsPath
addf entry = do 
  res <- Text.stripEnd <$> ipfst ["add","-rQ",format fp entry] empty
  return (IpfsPath res)

-- | Add a bytestring
adds :: Shell ByteString -> IpfsME IpfsPath
adds sh = do
  res <- Text.stripEnd <$> ipfst ["add","-Q"] sh
  return (IpfsPath res)

get :: IpfsPath -> IpfsME ()
get p = ipfs ["get",fIpfsPath p] empty >> return ()

-- | TODO: This should really return a 'Shell' 'ByteString'.
cat :: IpfsPath -> Turtle.FilePath -> IpfsME ByteString
cat p f = ipfs ["cat",format fp fullpath] empty
  where fullpath = fippFP p <> f

-- | Pin a list of 'IpfsPath's
pin :: [IpfsPath] -> IpfsME ()
pin ps = ipfs (["pin","add","--progress"] ++ map fIpfsPath ps) empty 
         >> return ()

putObject :: IpfsObject -> IpfsME IpfsPath
putObject (IpfsObject d links) = do
  basePath <- Text.strip <$> ipfst ["object","new"] empty
  let addLink root (n,IpfsPath p) = 
        Text.strip 
        <$> ipfst ["object","patch","add-link",root,Text.pack n,p] empty
  let apData = return (encodeUtf8 d)
  path' <- foldM addLink basePath (Map.toList links)
  p <- Text.strip <$> ipfst ["object","patch","append-data",path'] apData
  return (IpfsPath p)

getObject :: IpfsPath -> IpfsME IpfsObject
getObject (IpfsPath p) = do
  t <- fromStrict <$> ipfs ["object","get",p] empty
  case A.eitherDecode t of
    Right obj -> return obj
    Left msg -> throwError (Text.pack msg)
