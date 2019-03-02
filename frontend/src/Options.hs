{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleInstances #-}
module Options where

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T

import Common

data Options t = Options
    { optionsSize :: Dynamic t Size
    , optionsAllowDontCare :: Dynamic t Bool
    }

renderOptions :: (Reflex t, MonadHold t m, DomBuilder t m, PostBuild t m) => m (Options t)
renderOptions = el "div" $ mdo
    eSize <- renderOptionSize Small 
    eAllowDontCare <- renderOptionDontCare True

    dSize <- holdDyn Small eSize
    dAllowDontCare <- holdDyn True eAllowDontCare
    return $ Options dSize dAllowDontCare

renderOptionDontCare :: (Reflex t, DomBuilder t m, PostBuild t m) => Bool -> m (Event t Bool)
renderOptionDontCare initial = el "div" $ do
    text "Allow \"Don't Care\" States"
    allowDontCare <- checkbox initial def
    return $ _checkbox_change allowDontCare

renderOptionSize :: (Reflex t, DomBuilder t m) => Size -> m (Event t Size)
renderOptionSize initialSize = el "div" $ do
    text "Select size of K-map"
    eSize <- radioGroup initialSize showSize "size"
    return $ eSize

radioGroup :: (DomBuilder t m, Eq a, Enum a, Bounded a) => a -> (a -> Text) -> Text -> m (Event t a)
radioGroup initialValue toText groupName = elClass "div" "radioGroup" $ do
    events <- mapM (radioGroupOption groupName initialValue) options
    return $ leftmost events
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
