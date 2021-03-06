{-# LANGUAGE OverloadedStrings #-}
module Common where

import Data.Text (Text)
import qualified Data.Text as T

data Size = Small | Medium | Large
    deriving (Eq, Show, Enum, Bounded)

showSize :: Size -> Text
showSize Small = "2x2"
showSize Medium = "2x4"
showSize Large = "4x4"


data KMap = KMap2x2 | KMap2x4 | KMap4x4

getKMapFromSize :: Size -> KMap
getKMapFromSize Small = KMap2x2
getKMapFromSize Medium = KMap2x4
getKMapFromSize Large = KMap4x4

data Entry = ETrue | EFalse | DontCare

nextEntry :: Entry -> Entry
nextEntry EFalse = ETrue
nextEntry ETrue = DontCare
nextEntry DontCare = EFalse

displayEntry :: Entry -> Text
displayEntry ETrue = "1"
displayEntry EFalse = "0"
displayEntry DontCare = "X"
