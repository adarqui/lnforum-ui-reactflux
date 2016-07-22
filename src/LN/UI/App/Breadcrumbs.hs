{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Breadcrumbs (
  breadcrumbsView,
  breadcrumbsView_
) where



import           React.Flux                hiding (view)
import qualified React.Flux                as RF

import           LN.UI.ReactFlux.DOM
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Routes



breadcrumbsView :: ReactView RouteWith
breadcrumbsView = defineView "breadcrumbs" $ \(RouteWith route params) ->
  case (crumb route) of
    [] -> pure ()
    xs -> do
      div_ $ p_ $ do
        ol_ $ do
          mapM_ (\breadcrumb -> li_ $ ahref $ routeWith' breadcrumb) xs



breadcrumbsView_ :: RouteWith -> ReactElementM eventHandler ()
breadcrumbsView_ route_with =
  RF.view breadcrumbsView route_with mempty
