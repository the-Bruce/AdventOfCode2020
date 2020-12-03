module Today where

import Data.Time.Clock
import Data.Time.Calendar

date :: IO Int -- :: (year,month,day)
date = getCurrentTime >>= return . trd . toGregorian . utctDay

trd :: (Integer, Int, Int) -> Int
trd (_,12,x) = x
trd (_,_,_) = 0