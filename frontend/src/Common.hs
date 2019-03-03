{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Common where

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Map as Map
import Data.Maybe (catMaybes)

data Size = Small | Medium | Large
    deriving (Eq, Show, Enum, Bounded)

showSize :: Size -> Text
showSize Small = "2x2"
showSize Medium = "2x4"
showSize Large = "4x4"

data KMap = KMap
    { kmapSize :: Size
    , kmapEntries :: Map.Map Text Entry
    }

getEntriesForGrid :: KMap -> [Entry]
getEntriesForGrid (KMap size dict) = catMaybes $ (Map.!?) dict <$> getKeysFromSize size

getKeysFromSize :: Size -> [Text]
getKeysFromSize Small = [
    "00", "01",
    "10", "11"
    ]
getKeysFromSize Medium = [
    "000", "001",
    "010", "011",
    "110", "111",
    "100", "101"
    ]
getKeysFromSize Large = [
    "0000", "0001", "0011", "0010",
    "0100", "0101", "0111", "0110",
    "1100", "1101", "1111", "1110",
    "1000", "1001", "1011", "1010"
    ]

makeNewKMap :: Size -> KMap
makeNewKMap size = KMap size $ Map.fromList $ (,EFalse) <$> getKeysFromSize size

data Entry = ETrue | EFalse | DontCare

nextEntry :: Bool -> Entry -> Entry
nextEntry _ EFalse = ETrue
nextEntry True ETrue = DontCare
nextEntry False ETrue = EFalse
nextEntry _ DontCare = EFalse

displayEntry :: Entry -> Text
displayEntry ETrue = "1"
displayEntry EFalse = "0"
displayEntry DontCare = "X"
