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
import qualified LN.UI.ReactFlux.App.Boards           as Boards
import qualified LN.UI.ReactFlux.App.Boards           as Boards
import qualified LN.UI.ReactFlux.App.Breadcrumbs      as Breadcrumbs
import           LN.UI.ReactFlux.App.Core.Shared      as A
import qualified LN.UI.ReactFlux.App.Forums           as Forums
import qualified LN.UI.ReactFlux.App.Home             as Home
import qualified LN.UI.ReactFlux.App.NotFound         as NotFound (view_)
import qualified LN.UI.ReactFlux.App.Organizations    as Organizations
import qualified LN.UI.ReactFlux.App.Portal           as Portal
import qualified LN.UI.ReactFlux.App.ThreadPosts      as ThreadPosts
import qualified LN.UI.ReactFlux.App.Threads          as Threads
import qualified LN.UI.ReactFlux.App.Users            as Users
import qualified LN.UI.ReactFlux.App.Profile          as Profile
import qualified LN.UI.ReactFlux.Dispatcher           as Dispatcher
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM
import           LN.UI.ReactFlux.Types



initRouter :: IO ()
initRouter =
  initRouterRaw'ByteString (Just go) go
  where
  go = \raw_uri -> do
    print raw_uri
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
    defaultLayout st (renderRouteView st)



defaultLayout :: Store -> HTMLView_ -> HTMLView_
defaultLayout st@Store{..} page =
  div_ ["key" $= "default-layout"] $ do
    Loader.loader1 _l_m_me $ \m_me -> do
      navBar m_me _route
      Breadcrumbs.view _route
      div_ ["key" $= "page"] page



navBar :: Maybe UserResponse -> RouteWith -> HTMLView_
navBar m_user route_with =
  div_ ["key" $= "navbar", className_ B.containerFluid] $ do
    nav_ ["key" $= "nav", classNames_ [B.navbarNav, B.navbarStaticTop]] $ do
      div_ ["key" $= "nav-div-2", className_ B.container] $ do

        ahrefClassesKey "nav-home" [B.navbarBrand] $ routeWith' Home
        ul_ ["key" $= "nav-ul", classNames_ [B.navbarNav, B.nav, B.navTabs]] $ do
          li_ ["key" $= "nav-about"]  $ ahrefKey "nav-about" $ routeWith' About
          li_ ["key" $= "nav-portal"] $ ahrefKey "nav-portal" $ routeWith' Portal

          case m_user of
            Nothing               -> mempty
            Just UserResponse{..} -> do
              li_ ["key" $= "nav-me"] $ ahrefNameKey "nav-me" "Me" $ routeWith' $ Users (ShowS userResponseName)

          li_ ["key" $= "nav-user"]   $ do
            case m_user of
              Nothing               -> ahrefKey "nav-user" $ routeWith' Login
                                       -- Raw anchor here, to hit the server's /auth/logout
              Just UserResponse{..} -> a_ ["key" $= "nav-user", "href" $= "/auth/logout"] $ elemText ("Logout: " <> userResponseName)
          li_ ["key" $= "nav-refresh"] $ do
            -- A method for refreshing the current route, without actually refreshing the page from the browser
            --
            button_ [ classNames_ [B.btn, B.btnDefault, B.btnXs]
                    , onClick $ \_ _ -> dispatch $ Route route_with
                    ] $ span_ [classNames_ [B.glyphicon, B.glyphiconRefresh]] mempty




renderRouteView :: Store -> HTMLView_
renderRouteView Store{..} = do
  div_ $ do
    case _route of
      RouteWith Home _                        -> Home.view_
      RouteWith About _                       -> About.view_
      RouteWith Portal _                      -> Portal.view

      RouteWith (Organizations Index) _       -> Organizations.viewIndex _pageInfo _l_organizations
      RouteWith (Organizations New) _         -> Organizations.viewNew _m_organizationRequest
      RouteWith (Organizations (EditS _)) _   -> Organizations.viewEditS _m_organizationRequest _l_m_organization
      RouteWith (Organizations (ShowS _)) _   -> Organizations.viewShowS _pageInfo _l_m_organization _l_forums

      RouteWith (OrganizationsForums _ Index) _     -> Forums.viewIndex _pageInfo _l_m_organization _l_forums
      RouteWith (OrganizationsForums _ New) _       -> Forums.viewNew _l_m_organization _m_forumRequest
      RouteWith (OrganizationsForums _ (EditS _)) _ -> Forums.viewEditS _l_m_forum _m_forumRequest
      RouteWith (OrganizationsForums _ (ShowS _)) _ -> Forums.viewShowS _pageInfo _l_m_organization _l_m_forum _l_boards _l_recentThreadPosts

      RouteWith (OrganizationsForumsBoards _ _ Index) _     -> Boards.viewIndex _pageInfo _l_m_organization _l_m_forum _l_boards
      RouteWith (OrganizationsForumsBoards _ _ New) _       -> Boards.viewNew _l_m_forum _m_boardRequest
      RouteWith (OrganizationsForumsBoards _ _ (EditS _)) _ -> Boards.viewEditS _l_m_board _m_boardRequest
      RouteWith (OrganizationsForumsBoards _ _ (ShowS _)) _ -> Boards.viewShowS _pageInfo _l_m_organization _l_m_forum _l_m_board _l_threads

      RouteWith (OrganizationsForumsBoardsThreads _ _ _ Index) _     -> Threads.viewIndex _pageInfo _l_m_organization _l_m_forum _l_m_board _l_threads
      RouteWith (OrganizationsForumsBoardsThreads _ _ _ New) _       -> Threads.viewNew _l_m_board _m_threadRequest
      RouteWith (OrganizationsForumsBoardsThreads _ _ _ (EditS _)) _ -> Threads.viewEditS _l_m_thread _m_threadRequest
      RouteWith (OrganizationsForumsBoardsThreads _ _ _ (ShowS _)) _ -> Threads.viewShowS _pageInfo _meId _l_m_organization _l_m_forum _l_m_board _l_m_thread _l_threadPosts _m_threadPostRequest _usersCache

      RouteWith (OrganizationsForumsBoardsThreadsPosts _ _ _ _ Index) _     -> ThreadPosts.viewIndex _pageInfo _meId _l_m_organization _l_m_forum _l_m_board _l_m_thread _l_threadPosts _m_threadPostRequest _usersCache
      RouteWith (OrganizationsForumsBoardsThreadsPosts _ _ _ _ New) _       -> ThreadPosts.viewNew _l_m_thread _m_threadPostRequest
      RouteWith (OrganizationsForumsBoardsThreadsPosts _ _ _ _ (EditI _)) _ -> ThreadPosts.viewEditI _l_m_threadPost _m_threadPostRequest
      RouteWith (OrganizationsForumsBoardsThreadsPosts _ _ _ _ (ShowI _)) _ -> Threads.viewShowS _pageInfo _meId _l_m_organization _l_m_forum _l_m_board _l_m_thread _l_threadPosts _m_threadPostRequest _usersCache
--      RouteWith (OrganizationsForumsBoardsThreadsPosts _ _ _ _ (ShowI _)) _ -> ThreadPosts.viewIndex _pageInfo _meId _l_m_organization _l_m_forum _l_m_board _l_m_thread _l_threadPosts _m_threadPostRequest _usersCache
--      RouteWith (OrganizationsForumsBoardsThreadsPosts _ _ _ _ (ShowI _)) _ -> ThreadPosts.viewShowI _pageInfo _meId _l_m_organization _l_m_forum _l_m_board _l_m_thread _l_m_threadPost _usersCache

      RouteWith (Users Index) params          -> Users.viewIndex _pageInfo _l_users
      RouteWith (Users crud) params           -> Users.viewShowS _pageInfo _meId _l_m_user
      RouteWith (UsersProfile _ Index) _      -> Profile.viewIndex _meId _l_m_user
      RouteWith (UsersProfile _ EditZ) _      -> Profile.viewEditZ _l_m_user _m_profileRequest
      RouteWith _ _                           -> NotFound.view_
