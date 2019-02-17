{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Interface (mkUpdateChan, runUi) where

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

import Data.CARD
import Lang.Carol
import Lang.Carol.Bank

data CustomEvent = StoreUpdate Counter | GotMessage deriving Show

data CommandForm = CommandForm { _cfCommand :: Text.Text } deriving Show

data NetStat = Online | Offline deriving (Show,Read,Eq,Ord)

makeLenses ''CommandForm

data Name = CommandField deriving (Show,Read,Eq,Ord)

mkForm :: CommandForm -> Form CommandForm e Name
mkForm = newForm [ editTextField cfCommand CommandField (Just 1) ]

type St e = (NetStat, Counter, Form CommandForm e Name)

draw :: St e -> [Widget Name]
draw (ns,(Counter b),f) = 
  [C.vCenter $ C.hCenter form <=> C.hCenter store <=> C.hCenter status]
  where form = renderForm f
        store = Brick.str $ "Balance: $" <> (show b)
        status = Brick.str $ "Network status: " <> (show ns)

thd (_,_,a) = a

app :: (CCarrier c Counter IO) 
    => c
    -> App (St CustomEvent) CustomEvent Name
app cc = App 
  { appDraw = draw
  , appHandleEvent = \(ns,c,f) ev -> case ev of
      VtyEvent (V.EvResize {}) -> continue (ns,c,f)
      VtyEvent (V.EvKey V.KEsc []) -> halt (ns,c,f)
      VtyEvent (V.EvKey V.KEnter []) -> suspendAndResume $ do
        case Text.words ((formState f) ^.cfCommand) of
          ["dp",v] -> do 
            carolAsync' cc $ deposit (read . Text.unpack $ v)
            return (ns,c,mkForm $ CommandForm "")
          ["wd",v] -> do 
            carolAsync' cc $ withdraw (read . Text.unpack $ v)
            return (ns,c,mkForm $ CommandForm "")
          ["audit"] -> do c' <- carol cc currentS
                          return (ns,Counter c',mkForm $ CommandForm "")
          _ -> return (ns,c,f)
      AppEvent (StoreUpdate c') -> continue (ns,c',f)
      AppEvent GotMessage -> continue (Online,c,f)
      _ -> do
        f' <- handleFormEvent ev f
        continue (ns,c,f')
  , appChooseCursor = focusRingCursor (formFocus . thd)
  , appStartEvent = return
  , appAttrMap = const theMap }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow) ]

mkUpdateChan :: IO (BChan CustomEvent, Counter -> IO (), IO ())
mkUpdateChan = do
  chan <- newBChan 100
  return (chan, writeBChan chan . StoreUpdate, writeBChan chan GotMessage)

runUi :: (CCarrier c Counter IO) => Counter -> c -> BChan CustomEvent -> IO ()
runUi s0 conn chan = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      f = (Offline, s0, mkForm (CommandForm ""))
  f' <- customMain buildVty (Just chan) (app conn) f
  return ()
