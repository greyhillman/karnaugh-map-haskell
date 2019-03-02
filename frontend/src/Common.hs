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


data KMap = KMap Size [[Entry]]

makeNewKMap :: Size -> KMap
makeNewKMap Small = KMap Small [[EFalse, EFalse], [EFalse, EFalse]]

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
