{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | A Generic deletion view, with Ok and Cancel
--
module LN.UI.App.Delete (
  view_,
  view
) where



import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF

import           LN.UI.Helpers.ReactFluxDOM
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Route



view_ :: ReactElementM eventHandler ()
view_ =
  RF.view view () mempty



view :: ReactView ()
view = defineView "delete" $ \_ ->
  div_ $ p_ $ do
    h1_ "Delete"
