{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Helpers.GHCJS (
  JSString,
  textToJSString',
  targetValue
) where



import           Data.Text           (Text)
import qualified Data.Text           as Text

import           React.Flux
import           React.Flux.Internal (JSString)

#ifdef __GHCJS__
import qualified Data.JSString.Text  as JSS (textToJSString)
#endif



#ifdef __GHCJS__
textToJSString' :: Text -> JSString
textToJSString' = JSS.textToJSString
#else
textToJSString' :: Text -> String
textToJSString' = Text.unpack
#endif



-- TODO FIXME: Can't use FromJSVal because it's defined in React.Flux.PropertiesAndEvents but not exported
-- targetValue :: FromJSVal val => Event -> val
targetValue evt = target evt "value"
