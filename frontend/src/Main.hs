{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T

import Data.FileEmbed

main = mainWidgetWithCss ($(embedFile "main.css"))
    $ do
        renderHeader
        el "main" $ do
            renderOptions
            renderMap (KMap2x2 [[EFalse, ETrue], [DontCare, ETrue]])
        renderFooter

renderOptions :: (PostBuild t m, MonadHold t m, DomBuilder t m) => m ()
renderOptions = el "div" $ do
    el "div" $ do
        text "Select size of K-map"
        size <- radioGroup Small showSize "size"
        let sizeText = showSize <$> size
        dynText ("Size: " <> sizeText)
    el "div" $ do
        text "Allow \"Don't Care\" States"
        allowDontCare <- checkbox True def
        return ()

radioGroup :: (DomBuilder t m, MonadHold t m, Eq a, Enum a, Bounded a) => a -> (a -> Text) -> Text -> m (Dynamic t a)
radioGroup initialValue toText groupName = elClass "div" "radioGroup" $ do
    events <- mapM (radioGroupOption groupName initialValue) options
    holdDyn initialValue $ leftmost events
    where
        options = map toPair [minBound..maxBound]
        toPair x = (x, toText x)

radioGroupOption :: (DomBuilder t m, Eq a) => Text -> a -> (a, Text) -> m (Event t a)
radioGroupOption groupName initialValue (value, text') = elClass "div" "radioOption" $ do
    el "label" $ do
        text text'
    (e, _) <- elAttr' "input" attrs' blank
    return $ value <$ domEvent Click e
    where
        attrs = ("type" =: "radio") <> ("name" =: groupName)
        attrs' = if isChecked then attrs <> ("checked" =: "checked") else attrs
        isChecked = initialValue == value


data Size = Small | Medium | Large
    deriving (Eq, Show, Enum, Bounded)

showSize :: Size -> Text
showSize Small = "2x2"
showSize Medium = "2x4"
showSize Large = "4x4"

renderFooter :: DomBuilder t m => m ()
renderFooter = el "footer" $ do
    text "footer"

renderHeader :: DomBuilder t m => m ()
renderHeader = do
    el "header" $ do
        text "Karanaugh Map Generator"

renderMap :: DomBuilder t m => KMap -> m ()
renderMap map = do
    showKMap map

data KMap = KMap2x2 [[Entry]]

data Entry = ETrue | EFalse | DontCare

showKMap (KMap2x2 entries) = elClass "div" "map" $ do
    elClass "div" "top" $ text "X"
    elClass "div" "left" $ text "Y"
    elClass "div" "top-num" $ do
        el "div" $ text "0"
        el "div" $ text "1"
    elClass "div" "left-num" $ do
        el "div" $ text "0"
        el "div" $ text "1"
    elClass "div" "grid" $ do
        mapM_ (mapM_ showEntry) entries

showEntry entry = elClass "div" "entry" $ text (displayEntry entry)

clickableEntry entry = do
    (e, _) <- el' "div" (text "click")
    return $ domEvent Click e


displayEntry :: Entry -> Text
displayEntry ETrue = "1"
displayEntry EFalse = "0"
displayEntry DontCare = "X"
