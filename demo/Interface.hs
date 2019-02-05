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
import Network.Discard
import Lang.Carol.Bank

data CustomEvent = StoreUpdate Counter deriving Show

data CommandForm = CommandForm { _cfCommand :: Text.Text } deriving Show

makeLenses ''CommandForm

data Name = CommandField deriving (Show,Read,Eq,Ord)

mkForm :: CommandForm -> Form CommandForm e Name
mkForm = newForm [ editTextField cfCommand CommandField (Just 1) ]

draw :: (Counter, Form CommandForm e Name) -> [Widget Name]
draw ((Counter b),f) = [C.vCenter $ C.hCenter form <=> C.hCenter store]
  where form = renderForm f
        store = Brick.str $ "Balance: $" <> (show b)

app :: ManagerConn c i Counter
    -> App (Counter, Form CommandForm CustomEvent Name) CustomEvent Name
app conn = App 
  { appDraw = draw
  , appHandleEvent = \(c,f) ev -> case ev of
      VtyEvent (V.EvResize {}) -> continue (c,f)
      VtyEvent (V.EvKey V.KEsc []) -> halt (c,f)
      VtyEvent (V.EvKey V.KEnter []) -> suspendAndResume $ do
        case Text.words ((formState f) ^.cfCommand) of
          ["dp",v] -> do 
            runCarolM conn (const $ return ()) $ deposit (read . Text.unpack $ v)
            return (c,mkForm $ CommandForm "")
          ["wd",v] -> do 
            runCarolM conn (const $ return ()) $ withdraw (read . Text.unpack $ v)
            return (c,mkForm $ CommandForm "")
          ["audit"] -> do c' <- runCarolR conn $ currentS
                          return (Counter c',mkForm $ CommandForm "")
          _ -> return (c,f)
      AppEvent (StoreUpdate c') -> continue (c',f)
      _ -> do
        f' <- handleFormEvent ev f
        continue (c,f')
  , appChooseCursor = focusRingCursor (formFocus . snd)
  , appStartEvent = return
  , appAttrMap = const theMap }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow) ]

mkUpdateChan :: IO (BChan CustomEvent, Counter -> IO ())
mkUpdateChan = do
  chan <- newBChan 100
  return (chan, writeBChan chan . StoreUpdate)

runUi :: Counter -> ManagerConn c i Counter -> BChan CustomEvent -> IO ()
runUi s0 conn chan = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      f = (s0, mkForm (CommandForm ""))
  f' <- customMain buildVty (Just chan) (app conn) f
  return ()
