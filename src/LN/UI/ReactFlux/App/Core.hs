{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans  #-}

module LN.UI.ReactFlux.App.Core (
  module A,
  view_,
  view,
  initRouter
) where



import           Control.Concurrent                   (forkIO)
import           Control.DeepSeq                      (NFData)
import           Control.Monad                        (void)
import           Control.Monad.IO.Class               (liftIO)
import           Data.List                            ((\\))
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Monoid                          ((<>))
import           Data.Rehtie                          (rehtie)
import           Data.Text                            (Text)
import           Data.Typeable                        (Typeable)
import           GHC.Generics                         (Generic)
import           React.Flux                           hiding (view)
import qualified React.Flux                           as RF
import           React.Flux.Router.WebRoutes          (initRouterRaw'ByteString)
import qualified Web.Bootstrap3                       as B

import           LN.Api                               (getMe', getUserSanitizedPacks_ByUsersIds')
import           LN.T.Pack.Sanitized.User             (UserSanitizedPackResponse (..), UserSanitizedPackResponses (..))
import           LN.T.User                            (UserResponse (..))
import           LN.UI.Core.App                       (runCore)
import           LN.UI.Core.Control                   (CoreResult (..))
import           LN.UI.Core.Helpers.HaskellApiHelpers (rd)
import qualified LN.UI.Core.Loader                    as Loader (loader1)
import           LN.UI.Core.PageInfo                  (PageInfo,
                                                       defaultPageInfo)
import           LN.UI.Core.Router
import           LN.UI.Core.State                     (Action (..), Store (..),
                                                       defaultStore)
import qualified LN.UI.ReactFlux.App.About            as About
import qualified LN.UI.ReactFlux.App.Experiments      as Experiments
import qualified LN.UI.ReactFlux.App.Boards           as Boards
import qualified LN.UI.ReactFlux.App.Boards           as Boards
import qualified LN.UI.ReactFlux.App.Breadcrumbs      as Breadcrumbs
import           LN.UI.ReactFlux.App.Core.Shared      as A
import qualified LN.UI.ReactFlux.App.Home             as Home
import qualified LN.UI.ReactFlux.App.Layout           as Layout
import qualified LN.UI.ReactFlux.App.NavBar           as NavBar
import qualified LN.UI.ReactFlux.App.NotFound         as NotFound
import qualified LN.UI.ReactFlux.App.Portal           as Portal
import qualified LN.UI.ReactFlux.App.Profile          as Profile
import qualified LN.UI.ReactFlux.App.ThreadPosts      as ThreadPosts
import qualified LN.UI.ReactFlux.App.Threads          as Threads
import qualified LN.UI.ReactFlux.App.Users            as Users
import qualified LN.UI.ReactFlux.Dispatcher           as Dispatcher
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM
import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types



initRouter :: IO ()
initRouter =
  initRouterRaw'ByteString (Just go) go
  where
  go = \raw_uri -> do
    putStrLn $ "initRouter: " <> show raw_uri
    routeAlterStore $ toRouteWithHash raw_uri
    where
    routeAlterStore action =
      -- Update Store with our new route
      Dispatcher.dispatch $ SomeStoreAction store $ Route action



view_ :: HTMLEvent_
view_ =
  RF.view view () mempty



view :: ReactView ()
view =
  defineControllerView "core" store $ \st _ ->
    Layout.view st (renderRouteView st)



renderRouteView :: Store -> HTMLView_
renderRouteView !store' = do
  defineViewWithSKey "render-route-view" store' go
  where
  go :: Store -> HTMLView_
  go store@Store{..} = do
    case _route of
      RouteWith Home _                        -> Home.viewShowS _pageInfo _l_m_forum _l_boards _l_recentThreadPosts
      RouteWith About _                       -> About.view
      RouteWith Portal _                      -> Portal.view

      RouteWith (Boards Index) _     -> Boards.viewIndex _pageInfo  _l_boards
      RouteWith (Boards New) _       -> Boards.viewNew _l_m_forum _m_boardRequest
      RouteWith (Boards (EditS _)) _ -> Boards.viewEditS _l_m_board _m_boardRequest
      RouteWith (Boards (ShowS _)) _ -> Boards.viewShowS _pageInfo  _l_m_board _l_threads

      RouteWith (BoardsThreads _ Index) _     -> Threads.viewIndex _pageInfo  _l_m_board _l_threads
      RouteWith (BoardsThreads _ New) _       -> Threads.viewNew _l_m_board _m_threadRequest
      RouteWith (BoardsThreads _ (EditS _)) _ -> Threads.viewEditS _l_m_thread _m_threadRequest
      RouteWith (BoardsThreads _ (ShowS _)) _ -> Threads.viewShowS _pageInfo _meId  _l_m_board _l_m_thread _l_threadPosts _m_threadPostRequest _usersCache

      RouteWith (BoardsThreadsPosts _ _ Index) _     -> ThreadPosts.viewIndex _pageInfo _meId  _l_m_board _l_m_thread _l_threadPosts _m_threadPostRequest _usersCache
      RouteWith (BoardsThreadsPosts _ _ New) _       -> ThreadPosts.viewNew _l_m_thread _m_threadPostRequest
      RouteWith (BoardsThreadsPosts _ _ (EditI _)) _ -> ThreadPosts.viewEditI _l_m_threadPost _m_threadPostRequest
      RouteWith (BoardsThreadsPosts _ _ (ShowI _)) _ -> Threads.viewShowS _pageInfo _meId  _l_m_board _l_m_thread _l_threadPosts _m_threadPostRequest _usersCache
--      RouteWith (BoardsThreadsPosts _ _ _ _ (ShowI _)) _ -> ThreadPosts.viewIndex _pageInfo _meId  _l_m_board _l_m_thread _l_threadPosts _m_threadPostRequest _usersCache
--      RouteWith (BoardsThreadsPosts _ _ _ _ (ShowI _)) _ -> ThreadPosts.viewShowI _pageInfo _meId  _l_m_board _l_m_thread _l_m_threadPost _usersCache
--
--

      RouteWith (Users Index) params           -> Users.viewIndex _pageInfo _l_users
      RouteWith (Users crud) params            -> Users.viewShowS _pageInfo _meId _l_m_user
      RouteWith (UsersProfile _ Index) _       -> Profile.viewIndex _meId _l_m_user
      RouteWith (UsersProfile _ EditZ) _       -> Profile.viewEditZ _l_m_user _m_profileRequest
      RouteWith (Experiments experiment_sid) _ -> Experiments.view experiment_sid store
      RouteWith _ _                            -> NotFound.view
