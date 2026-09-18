module Test.MySolutions where

import Prelude 
import Data.Array (null, tail)
import Data.Maybe (fromMaybe)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven(n - 2)