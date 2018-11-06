{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as BC

import CARD
import CARD.Experiment
import CARD.EventGraph.Ipfs
import CARD.LQ.Bank

main :: IO ()
main = putStrLn "Hello world"

cmdGetter :: (Ord i, ToJSON i, FromJSON i) 
          => ((ExpConf, NetConf i) -> IO (Maybe Int))
          -> (Int -> IO Text)
          -> Application
cmdGetter hl hr request respond = do
  body <- strictRequestBody request
  case decode body of
    Just (Launch ec nc) -> hl (ec,nc) >>= \case
      Just n -> respond $ responseLBS status200 [] (BC.pack $ show n)
    Just (Report n) -> undefined
    Nothing -> respond $ responseLBS status400 [] "Could not parse"
