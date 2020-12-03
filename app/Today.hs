module Today where

import Data.Time.Clock
import Data.Time.Calendar

date :: IO Int -- :: (year,month,day)
date = getCurrentTime >>= return . trd . toGregorian . utctDay

trd :: (Integer, Int, Int) -> Int
trd (_,_,x) = x