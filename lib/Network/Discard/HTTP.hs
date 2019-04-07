module Network.Discard.HTTP
  ( msgListener
  , requestState
  , sendState
  
  ) where

import Control.Concurrent.STM
import Data.Aeson
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Network.Discard.Broadcast


-- | A WAI application that handles broadcast network messages.
--
-- The "handler action" is called for each received state broadcast or
-- state request.  The "rebroadcast action" is passed on to the
-- handler, which should call it if the received state contains new
-- information (and thus should be gossip'ed along).
msgListener :: (ToJSON s, FromJSON s) 
            => (BMsg s -> IO ()) -- ^ Handler action
            -> (s -> IO ()) -- ^ Rebroadcast action
            -> Wai.Application
msgListener = undefined

requestState :: (FromJSON s) => Client.Manager -> String -> IO (Maybe s)
requestState = undefined

sendState :: (ToJSON s) => Client.Manager -> String -> s -> IO ()
sendState = undefined

-- msgGetter :: (ToJSON (BMsg s), FromJSON (BMsg s)) 
--           => Int
--           -> (BMsg s -> IO (Bool, Maybe (BMsg s))) 
--           -> Application
-- msgGetter dbl handle request respond = do
--   dbg dbl 1 "Received HTTP message."
--   body <- strictRequestBody request
--   dbg dbl 2 (BS.unpack body)
--   -- Check for HELLO, otherwise parse as JSON
--   (_,mresp) <- case body of
--                  "HELLO" -> handle Hello
--                  _ -> case decode body of
--                         Just msg -> handle msg
--   case mresp of
--     Just r -> respond $ responseLBS status200 [] (encode r)
--     Nothing -> respond $ responseLBS status200 [] ""

-- instance (ToJSON (BMsg s), FromJSON (BMsg s)) => Carries HttpT s where
--   hello (HttpDest man dest) = do
--     let req = dest { Client.method = "GET"
--                    , Client.requestBody = Client.RequestBodyLBS $ "HELLO" }
--         sendRcv = do 
--           resp <- Client.httpLbs req man
--           return $ decode' (Client.responseBody resp)
--     catch sendRcv (\(Client.HttpExceptionRequest _ _) -> return Nothing)
--   send (HttpDest man dest) msg = do
--     let req = dest { Client.method = "POST"
--                    , Client.requestBody = Client.RequestBodyLBS $ encode msg }
--     catch (Client.httpLbs req man >> return ()) 
--           (\(Client.HttpExceptionRequest _ _) -> return ())
--     -- putStr "SEND: " >> print (encode msg)
--     return ()
--   listen dbl (HttpSrc p) handle = do
--     runSettings (setHost "!6" . setPort p $ defaultSettings) (msgGetter dbl handle)

-- -- | A 'NetConf' associates a 'String' hostname and 'Int' port number
-- -- to a set of 'i'-named replica nodes.
-- data NetConf i = NetConf (Map i (String, Int)) deriving (Show,Eq,Ord)

-- others :: (Ord i) => i -> NetConf i -> [(i,(String,Int))]
-- others i (NetConf m) = filter (\(i',_) -> i /= i') (Map.toList m)

-- self i (NetConf m) = Map.lookup i m

-- instance (ToJSON i) => ToJSON (NetConf i) where
--   toJSON (NetConf m) = 
--     toJSON (map ent $ Map.assocs m)
--     where ent (i,(h,p)) = object ["name" .= i
--                                  ,"host" .= h
--                                  ,"port" .= p]

-- instance (Ord i, FromJSON i) => FromJSON (NetConf i) where
--   parseJSON = withArray "NodeList" $ \v -> NetConf <$> do
--     foldM unpack Map.empty v
--     where unpack m = withObject "Node" $ \v -> do
--             name <- v .: "name"
--             host <- v .:? "host" .!= "localhost"
--             port <- v .:? "port" .!= defaultPort
--             return (Map.insert name (host,port) m)

-- defaultPort = 23001 :: Int

-- broadcast :: (Monad (Res t), Transport t, Carries t s) => [Dest t] -> s -> Res t ()
-- broadcast others s = mapM_ (flip send (BCast s)) others

-- -- | Send a 'Hello' message to all destinations
-- helloAll :: (Monad (Res t), Transport t, Carries t s) => [Dest t] -> Res t [s]
-- helloAll others = do 
--   resps <- mapM hello others
--   return . catMaybes $ map (\case
--                                Just (BCast s) -> Just s
--                                _ -> Nothing) 
--                            resps
