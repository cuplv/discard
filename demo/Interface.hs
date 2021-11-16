{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Interface (mkUpdateChan, runUi) where

import Bank

import Data.CARD.Capconf
import Data.CARD.Const
import Data.CARD.Counter
import Lang.CCRT
import Lang.CCRT.Token

import qualified Data.Text as Text

import Brick hiding (str)
import Brick.BChan
import Brick.Focus
import qualified Brick
import Brick.Forms
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Edit as E

import Lens.Micro.Platform

type CC = Capconf String (CounterC Int)

type Q = TokenMap String (CounterC Int)

data CustomEvent
  = StoreUpdate (Q,Int,CC)
  | GotMessage
  deriving Show

data CommandForm = CommandForm { _cfCommand :: Text.Text } deriving Show

data NetStat = Online | Offline deriving (Show,Read,Eq,Ord)

makeLenses ''CommandForm

data Name = CommandField deriving (Show,Read,Eq,Ord)

mkForm :: CommandForm -> Form CommandForm e Name
mkForm = newForm [ editTextField cfCommand CommandField (Just 1) ]

type St e = (NetStat, (Q,Int,CC), Form CommandForm e Name)

draw :: String -> St e -> [Widget Name]
draw i (ns,(q,c,r),f) = 
  [C.vCenter $ 
   C.hCenter (Brick.str $ "[ node " ++ i ++ " ]")
   <=> C.hCenter form
   <=> C.hCenter store
   <=> C.hCenter status
   <=> C.hCenter resStore
   <=> C.hCenter rqs]
  where form = renderForm f
        store = Brick.str $ "Balance: $" <> (show c)
        status = Brick.str $ "Network status: " <> (show ns)
        resStore = Brick.str $ 
          "R(" <> show (remoteG' i r) 
          <> ") W(" <> show (localG i r)
          <> ") -- " <> show r
        rqs = Brick.str $ "Rqs: " <> show q

prettyRes :: [CounterE Int] -> String
prettyRes (ModifyE e : es) = "+/- " ++ show (addAmt e) ++ " " ++ prettyRes es
-- prettyRes ((Effect [Sub n]) :es) = "-" ++ show n ++ " " ++ prettyRes es
prettyRes (e:es) = show e ++ " " ++ prettyRes es
prettyRes [] = ""

thd (_,_,a) = a

app :: String
    -> (BankOp' String IO -> IO ())
    -> App (St CustomEvent) CustomEvent Name
app i cc = App 
  { appDraw = draw i
  , appHandleEvent = \(ns,(q,c,r),f) ev -> case ev of
      VtyEvent (V.EvResize {}) -> continue (ns,(q,c,r),f)
      VtyEvent (V.EvKey V.KEsc []) -> halt (ns,(q,c,r),f)
      VtyEvent (V.EvKey V.KEnter []) -> suspendAndResume $ do
        case Text.words ((formState f) ^.cfCommand) of
          ["dp",v] -> do 
            cc.bankOp i $ depositT (read . Text.unpack $ v)
            return (ns,(q,c,r),mkForm $ CommandForm "")
          ["dpr",v] -> do
            rslt <- runTR cc (bankOp i) $ depositTR (read . Text.unpack $ v)
            case rslt of
              Right _ -> return (ns,(q,c,r),mkForm $ CommandForm "")
              Left e -> error $ "dp failed: " ++ show e ++ ", " ++ show (localG i r)
          ["wd",v] -> do
            -- rslt <- runTR cc (bankOp i) $ withdrawTR (read . Text.unpack $ v)
            -- case rslt of
            --   Right _ -> return (ns,(q,c,r),mkForm $ CommandForm "")
            --   Left e -> error $ "wd failed: " ++ show e
            cc.bankOp i $ withdrawT (read . Text.unpack $ v)
            return (ns,(q,c,r),mkForm $ CommandForm "")

          -- ["audit"] -> do c' <- carol cc currentS
          --                 return (ns,(c',r),mkForm $ CommandForm "")
          -- ["prod",v] -> do
          --   cc.tokenT $ depositR (read . Text.unpack $ v)
          --   return (ns,(c,r),mkForm $ CommandForm "")
          -- ["cons"] -> do
          --   carolAsync' cc withdrawR
          --   return (ns,(c,r),mkForm $ CommandForm "")
          _ -> return (ns,(q,c,r),f)
      AppEvent (StoreUpdate (q',c',r')) -> continue (ns,(q',c',r'),f)
      AppEvent GotMessage -> continue (Online,(q,c,r),f)
      _ -> do
        f' <- handleFormEvent ev f
        continue (ns,(q,c,r),f')
  , appChooseCursor = focusRingCursor (formFocus . thd)
  , appStartEvent = return
  , appAttrMap = const theMap }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow) ]

mkUpdateChan :: IO (BChan CustomEvent, (Q,Int,CC) -> IO (), IO ())
mkUpdateChan = do
  chan <- newBChan 100
  return (chan, writeBChan chan . StoreUpdate, writeBChan chan GotMessage)

runUi
  :: String
  -> Int
  -> (BankOp' String IO -> IO ())
  -> BChan CustomEvent
  -> IO ()
runUi nodeId s0 conn chan = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      f = (Offline, (mempty,s0,mempty), mkForm (CommandForm ""))
  vty0 <- buildVty
  f' <- customMain vty0 buildVty (Just chan) (app nodeId conn) f
  return ()
