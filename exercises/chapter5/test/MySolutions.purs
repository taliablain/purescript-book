module Test.MySolutions where

import Prelude
import Data.Array
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Alternative (guard)
import Test.Examples(factorsV3)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

countEven :: Array Int -> Int
countEven [] = 0
countEven arr = case head arr of
    Nothing -> 0
    Just x -> (if isEven x then 1 else 0) + countEven (fromMaybe [] (tail arr))

squared :: Array Number -> Array Number
squared arr = map (\n -> n * n) arr

keepNonNegative :: Array Number -> Array Number
keepNonNegative arr = filter (\n -> n >= 0.0) arr

isPrime :: Int -> Boolean 
isPrime n = n > 1 && length (factorsV3 n) == 1
-- isPrime n = n > 1 && ((length (factorsV3 n)) == 1)

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct arr1 arr2 = do 
    -- for each element of the first array, pair it with each element of the second array
    i <- arr1
    j <- arr2
    pure[i, j]
