module Lang.CCRT where

import Data.CARD.Classes

data CCRT c e s m
  = CCRT { ccrtRead :: c
         , ccrtWrite :: c
         , ccrtTrans :: s -> m e
         }
