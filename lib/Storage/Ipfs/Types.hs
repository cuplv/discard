{-# LANGUAGE OverloadedStrings #-}

module Storage.Ipfs.Types where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Yaml
import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader

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


data IpfsApi = DefIpfsApi | IpfsApi Text deriving (Eq,Ord)

type IpfsM = ReaderT IpfsApi IO


newtype IpfsPath = IpfsPath {ipfsPath :: Text} deriving (Eq,Ord)

instance Show IpfsPath where
  show = Text.unpack . fIpfsPath


-- | Parser for IPFS paths
pIpfsPath :: Parsec String st IpfsPath
pIpfsPath = 
  IpfsPath . Text.pack 
  <$> (P.string "/ipfs/" >> P.many P.alphaNum)

-- | Format an IPFS path as a text string
fIpfsPath :: IpfsPath -> Text
fIpfsPath (IpfsPath t) = "/ipfs/" <> t

uIpfsPath :: IpfsPath -> ByteString
uIpfsPath (IpfsPath t) = "/ipfs/" <> encodeUtf8 t

mkIpfsPath :: Text -> Either Text IpfsPath
mkIpfsPath t = case P.parse pIpfsPath "" (Text.unpack t) of
                 Right p -> Right p
                 Left e -> Left (Text.pack . show $ e)

unsafeIpfsPath :: Text -> IpfsPath
unsafeIpfsPath = IpfsPath

instance ToJSON IpfsPath where
  toJSON p = toJSON (fIpfsPath p)

instance ToJSONKey IpfsPath where
  toJSONKey = toJSONKeyText fIpfsPath

instance FromJSON IpfsPath where
  parseJSON = withText "IpfsPath" $ \t ->
    case mkIpfsPath t of
      Right p -> return p
      Left t -> fail (Text.unpack t)

instance FromJSONKey IpfsPath where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case mkIpfsPath t of
      Right p -> return p
      Left t -> fail (Text.unpack t)

data IpfsObject = IpfsObject
  { inodeData :: Text
  , inodeLinks :: Map FilePath IpfsPath }
  deriving (Show,Eq,Ord)

objData :: Text -> IpfsObject
objData t = IpfsObject t mempty

objLink :: FilePath -> IpfsPath -> IpfsObject
objLink name ref = IpfsObject mempty (Map.insert name ref mempty)

instance Semigroup IpfsObject where
  (<>) (IpfsObject d1 ls1) (IpfsObject d2 ls2) = IpfsObject (d1<>d2) (ls1<>ls2)

instance Monoid IpfsObject where
  mempty = IpfsObject mempty mempty

newtype IpfsLink = IpfsLink { unpackIpfsLink :: (FilePath, IpfsPath) }

instance FromJSON IpfsLink where
  parseJSON = withObject "IpfsLink" $ \v -> do
    f <- Text.unpack <$> v .: "Name"
    hash <- v .: "Hash"
    return $ IpfsLink (f,IpfsPath hash)

instance ToJSON IpfsLink where
  toJSON (IpfsLink (f,(IpfsPath p))) = 
    object ["Name" .= f
           ,"Hash" .= toJSON p]

instance FromJSON IpfsObject where
  parseJSON = withObject "IpfsObject" $ \v -> do
    ls <- v .: "Links" :: Parser [IpfsLink]
    d <- v .: "Data"
    return (IpfsObject d (Map.fromList . map unpackIpfsLink $ ls))

instance ToJSON IpfsObject where
  toJSON (IpfsObject d ls) = 
    object ["Data" .= toJSON d
           ,"Links" .= array (toJSON <$> IpfsLink <$> Map.toList ls)]
