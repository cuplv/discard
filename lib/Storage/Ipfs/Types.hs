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
-- import System.FilePath

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

data IpfsNode = IpfsNode
  { inodeData :: Text
  , inodeLinks :: Map FilePath IpfsPath }

newtype IpfsLink = IpfsLink { unpackIpfsLink :: (FilePath, IpfsPath) }

instance FromJSON IpfsLink where
  parseJSON = withObject "IpfsLink" $ \v -> do
    f <- Text.unpack <$> v .: "Name"
    cid <- v .: "Cid"
    hash <- cid .: "/"
    return $ IpfsLink (f,IpfsPath hash)

instance ToJSON IpfsLink where
  toJSON (IpfsLink (f,(IpfsPath p))) = 
    object ["Name" .= f
           ,"Cid" .= object ["/" .= toJSON p]]

instance FromJSON IpfsNode where
  parseJSON = withObject "IpfsNode" $ \v -> do
    ls <- v .: "links" :: Parser [IpfsLink]
    d <- v .: "data"
    return (IpfsNode d (Map.fromList . map unpackIpfsLink $ ls))

instance ToJSON IpfsNode where
  toJSON (IpfsNode d ls) = 
    object ["data" .= toJSON d
           ,"links" .= array (toJSON <$> IpfsLink <$> Map.toList ls)]
