module ID where

data ID
data Decision
defaultDecision :: Decision
isDefaultDecision :: Decision -> Bool

getKey :: ID -> Integer
getConsNr :: ID -> Int
nextNIDs :: ID -> Int -> [ID]

