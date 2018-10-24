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
  , ipfs
  , ipfst
  , addf
  , adds
  , get
  , Storage.Ipfs.Turtle.cat
  , pin

  -- * Helpful re-exports
  , throwError
  , liftEither

  ) where

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
import Data.ByteString.Lazy (toStrict)
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import Data.Map (Map)
import qualified Data.Map as Map

import Storage.Ipfs.Types

-- | The primary monad for interacting with IPFS processes,
-- incorporating an IPFS API connection and exception handling.
--
-- Basic usage:
--
-- > getData :: IO (Either Text ByteString)
-- > getData = withApi' "/ip4/127.0.0.1/tcp/5001" $ do
-- >   mainPath <- liftEither $ mkIpfsPath "/ipfs/QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG"
-- >   let subPath = fromText "readme"
-- >   cat mainPath subPath
type IpfsME = ExceptT Text IpfsM

l0 :: IpfsME a -> IpfsME a
l0 = id

l1 :: IpfsM a -> IpfsME a
l1 = lift

l2 :: IO a -> IpfsME a
l2 = lift.lift

lIpfsPath :: IpfsPath -> Line
lIpfsPath = unsafeTextToLine . fIpfsPath

fipp = fIpfsPath
fippFP = fromText . fipp


data IpfsApi = DefIpfsApi | IpfsApi Text deriving (Eq,Ord)

type IpfsM = ReaderT IpfsApi IO

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

-- getNode :: IpfsPath -> IpfsME IpfsNode
-- getNode p = do 
--   bs <- ipfs ["dag","get",fIpfsPath p] empty
--   case decodeEither' bs of
--     Right n -> return n
--     Left e -> throwError (Text.pack . show $ e)

-- -- | Put a node (warning: this doesn not put the correct size in the
-- -- node)
-- putNode :: IpfsNode -> IpfsME IpfsPath
-- putNode n = do
--   let bs = toStrict $ A.encode n
--   res <- Text.stripEnd <$> ipfst ["dag","put","--format","protobuf"] (return bs)
--   return (IpfsPath res)

-- -- | Wrap an ipfs node in a directory
-- nest :: IpfsPath -> FilePath -> IpfsME IpfsPath
-- nest p f = do 
--   putNode (IpfsNode "CAE=" (Map.fromList [(f,p)]))
