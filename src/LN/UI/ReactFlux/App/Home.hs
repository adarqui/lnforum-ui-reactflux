{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module LN.UI.ReactFlux.App.Home (
  viewShowS,
  viewMessagesOfTheWeek_,
  viewRecentPosts_
) where



import           Control.Concurrent                    (forkIO)
import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (forM_, void)
import           Control.Monad.Trans.Either            (EitherT, runEitherT)
import           Data.Ebyam                            (ebyam)
import           Data.Int                              (Int64)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Monoid                           ((<>))
import           Data.Rehtie                           (rehtie)
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Tuple.Select
import           Data.Typeable                         (Typeable)
import           GHC.Generics                          (Generic)
import           Haskell.Helpers.Either                (mustPassT)
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF
import qualified Web.Bootstrap3                        as B

import           LN.Api
import qualified LN.Api.String                         as ApiS
import           LN.Sanitize.Internal                  (toSafeUrl)
import           LN.T.Board
import           LN.T.Convert                          (forumResponseToForumRequest)
import           LN.T.Forum
import           LN.T.Pack.Board
import           LN.T.Pack.Forum
import           LN.T.Pack.ThreadPost
import           LN.T.Param
import           LN.T.Size
import           LN.T.Thread
import           LN.T.ThreadPost
import           LN.T.User
import qualified LN.UI.Core.App.Forum                  as Forum
import           LN.UI.Core.Helpers.DataList           (deleteNth)
import           LN.UI.Core.Helpers.DataText           (tshow)
import           LN.UI.Core.Helpers.DataTime           (prettyUTCTimeMaybe)
import           LN.UI.Core.Helpers.HaskellApiHelpers  (rd)
import           LN.UI.Core.Helpers.Map                (idmapFrom)
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
import           LN.UI.ReactFlux.Access
import qualified LN.UI.ReactFlux.App.Boards            as Boards
import           LN.UI.ReactFlux.App.Core.Shared
import qualified LN.UI.ReactFlux.App.Delete            as Delete
import qualified LN.UI.ReactFlux.App.Gravatar          as Gravatar
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loading
import qualified LN.UI.ReactFlux.App.NotFound          as NotFound
import qualified LN.UI.ReactFlux.App.Oops              as Oops
import           LN.UI.ReactFlux.App.PageNumbers       (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers       as PageNumbers
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM  (ahref, ahrefClasses,
                                                        ahrefClassesName,
                                                        ahrefName, className_,
                                                        classNames_)
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.View.Button
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal



viewShowS
  :: PageInfo
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Map BoardId BoardPackResponse)
  -> Loader (Map ThreadPostId ThreadPostPackResponse)
  -> HTMLView_

viewShowS !page_info !l_m_forum !l_boards !l_recent_posts = do
  defineViewWithSKey "forums-show-1" (page_info, l_m_forum, l_boards, l_recent_posts) go
  where
  go (page_info', l_m_forum', l_boards', l_recent_posts') = do

    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ do

        Loading.loader1 l_m_forum' $ \m_forum -> do
          case m_forum of
            Nothing -> mempty
            Just forum -> do
              let
                ForumPackResponse{..}        = forum
                ForumResponse{..}            = forumPackResponseForum

              h2_ $ elemText forumResponseDisplayName
              p_ [className_ B.lead] $ elemText $ maybe "No description." id forumResponseDescription

              -- ACCESS: Forum
              -- * Create: can create boards within a forum
              -- * Update: can edit forum settings
              -- * Delete: can delete the forum
              --
              buttonGroup_HorizontalSm1 $ do
                permissionsHTML'
                  forumPackResponsePermissions
                  (button_newBoard $ routeWith' $ Boards New)
                  permReadEmpty
                  (button_editForum $ routeWith' $ Forums (EditS forumResponseName))
                  (button_deleteForum $ routeWith' $ Forums (DeleteS forumResponseName))
                  permExecuteEmpty

        Boards.viewIndex page_info' l_boards'

        viewRecentPosts l_recent_posts

        viewMessagesOfTheWeek l_recent_posts

        viewForumStats CantLoad

        viewUsersOnline CantLoad


-- viewNew
--   -> Maybe ForumRequest
--   -> HTMLView_

-- viewNew !m_request = do
--   Loading.maybeLoader1 $ \OrganizationPackResponse{..} ->
--     ebyam m_request mempty $ \request -> viewMod TyCreate organizationPackResponseOrganizationId Nothing request



-- viewEditS
--   :: Loader (Maybe ForumPackResponse)
--   -> Maybe ForumRequest
--   -> HTMLView_

-- viewEditS !l_m_forum !m_request =
--   Loading.maybeLoader1 l_m_forum $ \ForumPackResponse{..} ->
--     ebyam m_request mempty $ \request -> viewMod TyUpdate (forumResponseOrgId forumPackResponseForum) (Just forumPackResponseForumId) request



viewMod
  :: TyCRUD
  -> Maybe ForumId
  -> ForumRequest
  -> HTMLView_

viewMod !tycrud' !m_forum_id' !request' = do
  defineViewWithSKey "forums-mod" (tycrud', m_forum_id', request') go

  where
  go :: (TyCRUD, Maybe ForumId, ForumRequest) -> HTMLView_
  go (tycrud, m_forum_id, request) = do

    let
      ForumRequest{..} = request

    div_ $ do
      h1_ $ elemText $ linkName tycrud <> " Forum"

      mandatoryNameField forumRequestDisplayName (dispatch . Forum.setDisplayName request)

      renderedText "Safe name: " (toSafeUrl forumRequestDisplayName)

      optionalDescriptionField forumRequestDescription
        (dispatch . Forum.setDescription request)
        (dispatch $ Forum.clearDescription request)

      mandatoryIntegerField "Threads per Board" forumRequestThreadsPerBoard 20 10 50 10
        (dispatch . Forum.setThreadsPerBoard request)

      mandatoryIntegerField "Posts per Thread" forumRequestThreadPostsPerThread 20 10 50 10
        (dispatch . Forum.setThreadPostsPerThread request)

      mandatoryIntegerField "Recent threads (limit)" forumRequestRecentThreadsLimit 10 0 20 1
        (dispatch . Forum.setRecentThreadsLimit request)

      mandatoryIntegerField "Recent posts (limit)" forumRequestRecentPostsLimit 10 0 20 1
        (dispatch . Forum.setRecentPostsLimit request)

      mandatoryIntegerField "Messages of the week (limit)" forumRequestMotwLimit 10 0 20 1
        (dispatch . Forum.setMotwLimit request)

      mandatoryVisibilityField forumRequestVisibility
        (dispatch . Forum.setVisibility request)

      tagsField
        forumRequestTags
        -- TODO FIXME: (maybe "" id forumRequestStateTag)
        ""
        (dispatch . Forum.setTag request)
        (dispatch $ Forum.addTag request)
        (dispatch . Forum.deleteTag request)
        (dispatch $ Forum.clearTags request)

      createButtonsCreateEditCancel
        m_forum_id
        (dispatch Save)
        (const $ dispatch Save)
        (routeWith' Home)



--
-- Re: ADARQ's Journal by adarqui (Progress Journals & Experimental Routines) Today at 06:00:30 pm
--
viewMessagesOfTheWeek
  :: Loader (Map ThreadPostId ThreadPostPackResponse)
  -> HTMLView_

viewMessagesOfTheWeek l_posts_map = do
  defineViewWithSKey "forums-messages-of-the-week" (l_posts_map) $ \l_posts_map' -> do
    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ do
        h4_ $ elemText "Messages of the week"
        Loading.loader1 l_posts_map' $ \posts_map -> do
          viewMessagesOfTheWeek_ posts_map

viewMessagesOfTheWeek_
  :: Map ThreadPostId ThreadPostPackResponse
  -> HTMLView_

viewMessagesOfTheWeek_ posts_map = do
  defineViewWithSKey "forums-messages-of-the-week_" (posts_map) go
  where
  go posts_map' = do
    p_ $ elemText "TODO FIXME: messages of the week"



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



--
-- Forum Stats
-- 144382 Posts in 5532 Topics by 460 Members. Latest Member: fitnessvolts
-- Latest Post: "Re: The adarq.org forum ..." ( Today at 11:27:47 am )
-- View the most recent posts on the forum.
-- [More Stats]
--
viewForumStats
  :: Loader (Map ThreadPostId ThreadPostPackResponse)
  -> HTMLView_

viewForumStats l_posts_map = do
  defineViewWithSKey "forums-stats-1" (l_posts_map) $ \l_posts_map' -> do
    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ do
        h4_ $ elemText "Forum Stats"
        Loading.loader1 l_posts_map' $ \posts_map -> do
          viewForumStats_ posts_map

viewForumStats_
  :: Map ThreadPostId ThreadPostPackResponse
  -> HTMLView_

viewForumStats_ posts_map = do
  defineViewWithSKey "forums-stats-2" (posts_map) go
  where
  go posts_map' = do
    p_ $ elemText "TODO FIXME: forum stats"



--
-- Users Online
-- 1 Guest, 1 User
-- Users active in past 15 minutes:
-- adarqui
-- Most Online Today: 4. Most Online Ever: 219 (September 14, 2012, 04:53:02 pm)
--
viewUsersOnline
  :: Loader (Map ThreadPostId ThreadPostPackResponse)
  -> HTMLView_

viewUsersOnline l_posts_map = do
  defineViewWithSKey "users-online-1" (l_posts_map) $ \l_posts_map' -> do
    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ do
        h4_ $ elemText "Users Online"
        Loading.loader1 l_posts_map' $ \posts_map -> do
          viewUsersOnline_ posts_map

viewUsersOnline_
  :: Map ThreadPostId ThreadPostPackResponse
  -> HTMLView_

viewUsersOnline_ posts_map = do
  defineViewWithSKey "users-online-2" (posts_map) go
  where
  go posts_map' = do
    p_ $ elemText "TODO FIXME: users-online"
