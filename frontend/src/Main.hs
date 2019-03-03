{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Fix
import qualified Data.Map as Map

import Data.FileEmbed

import Options
import Common

main = mainWidgetWithCss ($(embedFile "static/main.css"))
    $ do
        renderHeader
        kmap <- el "main" $ mdo
            options <- renderOptions
            dEntries <- renderMap options kmap
            kmap <- holdKMap options (makeNewKMap Small) dEntries
            renderMap options kmap
            return kmap
        renderFooter kmap
        return ()

holdKMap :: (Reflex t, MonadHold t m, MonadFix m) => Options t -> KMap -> Dynamic t (Map.Map Text Entry) -> m (Dynamic t KMap)
holdKMap options initialKMap dEntries = do
    let eSize = updated $ optionsSize options
    kmap <- foldDyn ($) initialKMap $ mergeWith (.) $ [
        const . makeNewKMap <$> eSize
        ]
    return kmap

renderFooter :: (PostBuild t m, DomBuilder t m) => Dynamic t KMap -> m ()
renderFooter dKMap = el "footer" $ do
    let entries = getEntriesForGrid <$> dKMap
    let entriesText = (displayEntry <$>) <$> entries
    el "div" $ do
        dynText $ T.intercalate " " <$> entriesText
    text "footer"

renderHeader :: DomBuilder t m => m ()
renderHeader = do
    el "header" $ do
        text "Karanaugh Map Generator"

renderMap :: (MonadSample t m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Options t -> Dynamic t KMap -> m (Dynamic t (Map.Map Text Entry))
renderMap options dMap = do
    map <- sample $ current dMap
    return $ constDyn Map.empty 

showKMap options kmap@(KMap size _) = elClass "div" "map" $ do
    elClass "div" "top" $ text "X"
    elClass "div" "left" $ text "Y"
    elClass "div" "top-num" $ do
        el "div" $ text "0"
        el "div" $ text "1"
    elClass "div" "left-num" $ do
        el "div" $ text "0"
        el "div" $ text "1"
    elClass "div" "grid" $ do
        let dAllowDontCare = optionsAllowDontCare options
        let entries = getEntriesForGrid kmap
        dEntries <- mapM (changeableMapEntry options) entries
        return ()

renderEntry :: (DomBuilder t m, PostBuild t m) => Dynamic t Entry -> m (Event t ())
renderEntry dEntry = do
    (e, _) <- elClass' "div" "entry" $ do
        dynText $ displayEntry <$> dEntry
    return $ domEvent Click e

changeableMapEntry :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Options t -> Entry -> m (Dynamic t Entry)
changeableMapEntry options initialEntry = mdo
    eClick <- renderEntry entry
    entry <- holdEntry options initialEntry eClick
    return $ entry

holdEntry :: (Reflex t, MonadHold t m, MonadFix m) => Options t -> Entry -> Event t () -> m (Dynamic t Entry)
holdEntry options initialEntry eClick = mdo
    let dAllowDontCare = optionsAllowDontCare options
    let bAllowDontCare = current dAllowDontCare
    let changeEntry = nextEntry <$> bAllowDontCare <@ eClick

    entry <- foldDyn ($) initialEntry changeEntry
    return entry

