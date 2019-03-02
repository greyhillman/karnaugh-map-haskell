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

import Data.FileEmbed

import Options
import Common

main = mainWidgetWithCss ($(embedFile "static/main.css"))
    $ do
        renderHeader
        el "main" $ do
            options <- renderOptions
            let dKMap = makeNewKMap <$> (optionsSize options)
            renderMap options dKMap
        renderFooter
        return ()

renderFooter :: DomBuilder t m => m ()
renderFooter = el "footer" $ do
    text "footer"

renderHeader :: DomBuilder t m => m ()
renderHeader = do
    el "header" $ do
        text "Karanaugh Map Generator"

renderMap :: (MonadSample t m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Options t -> Dynamic t KMap -> m ()
renderMap options dMap = do
    map <- sample $ current dMap
    showKMap options map

showKMap options (KMap size entries) = elClass "div" "map" $ do
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
        mapM_ (mapM_  $ mapEntry dAllowDontCare) entries

showEntry entry = elClass "div" "entry" $ text (displayEntry entry)

renderEntry :: (DomBuilder t m, PostBuild t m) => Dynamic t Entry -> m (Event t ())
renderEntry dEntry = do
    (e, _) <- elClass' "div" "entry" $ do
        dynText $ displayEntry <$> dEntry
    return $ domEvent Click e

mapEntry :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Dynamic t Bool -> Entry -> m (Event t Entry)
mapEntry dAllowDontCare initialEntry = mdo
    eClick <- renderEntry entry
    let bAllowDontCare = current dAllowDontCare
    let changeEntry = nextEntry <$> bAllowDontCare <@ eClick
    entry <- foldDyn ($) initialEntry changeEntry
    return $ updated entry


