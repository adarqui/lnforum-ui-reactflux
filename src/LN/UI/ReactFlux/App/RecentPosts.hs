{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.UI.ReactFlux.App.RecentPosts (
  viewRecentPosts,
  viewRecentPosts_
) where



import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Monoid                           ((<>))
import           React.Flux                            hiding (view)
import qualified Web.Bootstrap3                        as B

import           Control.Monad                         (forM_, void)
import           LN.T.Board
import           LN.T.Pack.ThreadPost
import           LN.T.Pack.User
import           LN.T.Param
import           LN.T.Size                             (Size (..))
import           LN.T.Thread
import           LN.T.ThreadPost
import           LN.T.User
import           LN.UI.Core.Helpers.DataText           (tshow)
import           LN.UI.Core.Helpers.DataTime           (prettyUTCTimeMaybe)
import           LN.UI.Core.PageInfo                   (PageInfo (..),
                                                        defaultPageInfo,
                                                        pageInfoFromParams,
                                                        paramsFromPageInfo)
import           LN.UI.Core.Router                     (CRUD (..), Params,
                                                        Route (..),
                                                        RouteWith (..),
                                                        TyCRUD (..),
                                                        emptyParams, linkName,
                                                        routeWith, routeWith')
import           LN.UI.Core.Sort
import qualified LN.UI.ReactFlux.App.Gravatar          as Gravatar
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loading
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM  (ahref, ahrefClasses,
                                                        ahrefClassesName,
                                                        ahrefName, className_,
                                                        classNames_)
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types



--
-- Re: ADARQ's Journal by adarqui (Progress Journals & Experimental Routines) Today at 06:00:30 pm
--
viewRecentPosts
  :: Loader (Map ThreadPostId ThreadPostPackResponse)
  -> HTMLView_

viewRecentPosts !l_posts_map = do
  defineViewWithSKey "view-recent-posts-1" (l_posts_map) $ \l_posts_map' -> do
    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ h4_ $ elemText "Recent posts"
      Loading.loader1 l_posts_map' $ \posts_map -> do
        viewRecentPosts_ posts_map



viewRecentPosts_
  :: Map ThreadPostId ThreadPostPackResponse
  -> HTMLView_

viewRecentPosts_ !posts_map = do
  defineViewWithSKey "view-recent-posts-2" (posts_map) $ \posts_map' -> do
    ul_ [className_ B.listUnstyled] $ do
      forM_ (sortThreadPostPacks SortOrderBy_Dsc posts_map') $ \pack@ThreadPostPackResponse{..} -> do
        let
          post@ThreadPostResponse{..} = threadPostPackResponseThreadPost
          m_board = threadPostPackResponseWithBoard
          m_thread = threadPostPackResponseWithThread
          board_name = maybe "unknown" boardResponseName m_board
          thread_name = maybe "unknown" threadResponseName m_thread
          user@UserSanitizedResponse{..} = threadPostPackResponseUser
        li_ $ do
          p_ $ do
            Gravatar.viewUser XSmall threadPostPackResponseUser
            elemText " "
            ahrefName (thread_name <> "/" <> tshow threadPostResponseId) $ routeWith' (BoardsThreadsPosts board_name thread_name (ShowI threadPostResponseId))
            elemText " by "
            ahref $ routeWith' (Users (ShowS userSanitizedResponseName))
            elemText " ("
            ahrefName board_name $ routeWith' (Boards (ShowS board_name))
            elemText ") at "
            elemText $ prettyUTCTimeMaybe threadPostResponseCreatedAt
