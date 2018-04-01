module Utils(showDate) where

import Data.Hourglass

showDate :: Date -> String
showDate (Date y m d) = show d ++ " " ++ show m ++ " " ++ show y
