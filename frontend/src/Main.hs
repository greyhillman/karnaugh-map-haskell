{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T

import Data.FileEmbed

main = mainWidgetWithCss (
    $(embedFile "frontend/static/main.css") )
        $ elClass "div" "map-container" $ do
        showKMap (KMap2x2 [EFalse, ETrue, DontCare, ETrue])


data KMap = KMap2x2 [Entry]

data Entry = ETrue | EFalse | DontCare

showKMap (KMap2x2 entries)= elClass "div" "map" $ do
    elClass "div" "top" $ text "X"
    elClass "div" "left" $ text "Y"
    elClass "div" "top-num" $ do
        el "div" $ text "0"
        el "div" $ text "1"
    elClass "div" "left-num" $ do
        el "div" $ text "0"
        el "div" $ text "1"
    elClass "div" "grid" $ do
        mapM_ showEntry entries

showEntry entry = elClass "div" "entry" $ text (displayEntry entry)


displayEntry :: Entry -> Text
displayEntry ETrue = "1"
displayEntry EFalse = "0"
displayEntry DontCare = "X"
