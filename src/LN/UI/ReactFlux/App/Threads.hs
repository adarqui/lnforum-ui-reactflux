{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Threads (
  viewIndex,
  viewIndex_,
  viewNew,
  viewEditS,
  viewShowS
) where



import           Control.Concurrent                    (forkIO)
import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (forM_, void)
import           Control.Monad.Trans.Either            (EitherT, runEitherT)
import           Data.Ebyam                            (ebyam)
import           Data.Int                              (Int64)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (fromJust, isJust)
import           Data.Monoid                           ((<>))
import           Data.Rehtie                           (rehtie)
import           Data.Text                             (Text)
import           Data.Tuple.Select
import           Data.Typeable                         (Typeable)
import           GHC.Generics                          (Generic)
import           Haskell.Helpers.Either                (mustPassT)
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF
import qualified Web.Bootstrap3                        as B

import           LN.Api
import qualified LN.Api.String                         as ApiS
import           LN.Generate.Default                   (defaultBoardRequest)
import           LN.Sanitize.Internal                  (toSafeUrl)
import           LN.T.Board
import           LN.T.Convert
import           LN.T.Forum
import           LN.T.Pack.Board
import           LN.T.Pack.Forum
import           LN.T.Pack.Sanitized.User
import           LN.T.Pack.Thread
import           LN.T.Pack.ThreadPost
import           LN.T.Param
import           LN.T.Size
import           LN.T.Thread
import           LN.T.ThreadPost
import           LN.T.User
import qualified LN.UI.Core.App.Thread                 as Thread
import           LN.UI.Core.Helpers.DataList           (deleteNth)
import           LN.UI.Core.Helpers.DataText           (tshow)
import           LN.UI.Core.Helpers.DataTime           (prettyUTCTimeMaybe)
import           LN.UI.Core.Helpers.GHCJS              (showToJSString')
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
import           LN.UI.ReactFlux.App.Core.Shared
import qualified LN.UI.ReactFlux.App.Delete            as Delete
import qualified LN.UI.ReactFlux.App.Gravatar          as Gravatar
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loader
import qualified LN.UI.ReactFlux.App.NotFound          as NotFound
import qualified LN.UI.ReactFlux.App.Oops              as Oops
import           LN.UI.ReactFlux.App.PageNumbers       (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers       as PageNumbers
import qualified LN.UI.ReactFlux.App.ThreadPosts       as ThreadPosts
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM  (ahref, ahrefClasses,
                                                        ahrefClassesName,
                                                        ahrefName, className_,
                                                        classNames_)
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.View.Button
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal




viewIndex
  :: PageInfo
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Map ThreadId ThreadPackResponse)
  -> HTMLView_

viewIndex !page_info' !l_m_board' !l_threads' = do
  defineViewWithSKey "threads-index-1" (page_info', l_m_board', l_threads') go
  where
  go (page_info, l_m_board, l_threads) = do
    h1_ [className_ B.textCenter] $ elemText "Threads"
    Loader.maybeLoader1 l_m_board $ \board -> do
      Loader.loader1 l_threads $ \threads -> do
        viewIndex_ page_info board threads



viewIndex_
  :: PageInfo
  -> BoardPackResponse
  -> Map ThreadId ThreadPackResponse
  -> HTMLView_

viewIndex_ !page_info' !board' !threads_map' = do
  defineViewWithSKey "threads-index-2" (page_info', board', threads_map') go
  where
  go (page_info, board, threads_map) = do

    let
      BoardPackResponse{..}        = board
      BoardResponse{..}            = boardPackResponseBoard

    PageNumbers.view1 page_info (routeWith' $ Boards (ShowS boardResponseName))
    ul_ [className_ B.listUnstyled] $ do
      -- TODO FIXME: This is good actually.. frontend shouldn't show threads with no posts.
      -- We also shouldn't allow threads to be created without posts.. that's another issue
      --
      forM_ (sortThreadPacks SortOrderBy_Dsc threads_map) $ \ThreadPackResponse{..} -> do
        let
          ThreadResponse{..}        = threadPackResponseThread
          ThreadStatResponse{..}    = threadPackResponseStat
          post                      = threadPackResponseLatestThreadPost
          latest_post_user          = fromJust threadPackResponseLatestThreadPostUser
        li_ $ do
          cldiv_ B.row $ do
            cldiv_ B.colXs2 $ do
              -- TODO FIXME: add link to user name
              -- p_ $ ahref ...
              p_ $ ahref $ routeWith' $ Users (ShowS (userSanitizedResponseName threadPackResponseUser))
              Gravatar.viewUser Small threadPackResponseUser
            cldiv_ B.colXs4 $ do
              p_ $ ahrefName threadResponseDisplayName $ routeWith' $ BoardsThreads boardResponseName (ShowS threadResponseName)
              p_ $
                PageNumbers.viewAbbreviated_
                  ("page-numbers-abbrv-" <> showToJSString' threadResponseId)
                  (defaultPageInfo { totalPages = threadStatResponseThreadPosts })
                  (routeWith' $ BoardsThreads boardResponseName (ShowS threadResponseName))

              p_ $ elemText $ prettyUTCTimeMaybe threadResponseCreatedAt
            cldiv_ B.colXs2 $ do
              showTableClean
                []
                ["posts", "views"]
                [[threadStatResponseThreadPosts]
                ,[threadStatResponseViews]
                ]
            cldiv_ B.colXs3 $ do
              case post of
                Just ThreadPostResponse{..} -> do
                  div_ $ do
                    p_ $ do
                      elemText "Last "
                      ahrefName "post" $ routeWith' $ BoardsThreadsPosts boardResponseName threadResponseName (ShowI threadPostResponseId)
                      elemText " by "
                      ahref $ routeWith' $ Users (ShowS (userSanitizedResponseName latest_post_user))
                      elemText " "
                      Gravatar.viewUser XSmall (fromJust threadPackResponseLatestThreadPostUser)
                _ -> div_ $ p_ $ elemText "No posts."
            cldiv_ B.colXs1 $ do
              cldiv_ B.container $ do
                buttonGroup_VerticalSm1 $ do
                  -- ACCESS: Thread
                  -- * Update: edit thread & thread settings
                  -- * Delete: delete thread
                  --
                  permissionsHTML'
                    threadPackResponsePermissions
                    permCreateEmpty
                    permReadEmpty
                    (button_editThread $ routeWith' $ BoardsThreads boardResponseName (EditS threadResponseName))
                    (button_deleteThread $ routeWith' $ BoardsThreads boardResponseName (DeleteS threadResponseName))
                    permExecuteEmpty

    PageNumbers.view2 page_info (routeWith' $ Boards (ShowS boardResponseName))



viewShowS
  :: PageInfo
  -> UserId
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Maybe ThreadPackResponse)
  -> Loader (Map ThreadId ThreadPostPackResponse)
  -> Maybe ThreadPostRequest
  -> Map UserId UserSanitizedPackResponse
  -> HTMLView_

viewShowS !page_info' !me_id' !l_m_board' !l_m_thread' !l_posts' !m_request' !users_map' = do
  defineViewWithSKey "threads-show-1" (page_info', me_id', l_m_board', l_m_thread', l_posts', m_request', users_map') go
  where
  go (page_info, me_id, l_m_board, l_m_thread, l_posts, m_request, users_map) = do

    Loader.loader3 l_m_board l_m_thread l_posts $ \m_board m_thread posts -> do
      case (m_board, m_thread) of
        (Just board, Just thread) ->
          viewShowS_
            page_info
            board
            thread
            (ThreadPosts.viewIndex_ page_info me_id board thread posts m_request users_map)
        _ -> Oops.view



viewShowS_
  :: PageInfo
  -> BoardPackResponse
  -> ThreadPackResponse
  -> HTMLView_ -- ^ plumbing thread posts
  -> HTMLView_

viewShowS_ !page_info' !board' !thread' !plumbing_posts' = do
  defineViewWithSKey
    "threads-show-2"
    (page_info', board', thread', plumbing_posts')
    go

  where
  go (page_info, board, thread, plumbing_posts) = do

    let
      BoardPackResponse{..}        = board
      BoardResponse{..}            = boardPackResponseBoard
      ThreadPackResponse{..}       = thread
      ThreadResponse{..}           = threadPackResponseThread

    -- h2_ $ elemText threadResponseDisplayName
    cldiv_ B.containerFluid $ do
      -- cldiv_ B.pageHeader $ do
        -- buttonGroup_HorizontalSm1 $ do
        --   -- ACCESS:
        --   -- * Update: edit thread settings
        --   -- * Delete: delete thread settings
        --   --
        --   permissionsHTML'
        --     threadPackResponsePermissions
        --     permCreateEmpty
        --     permReadEmpty
        --     (button_editThread $ routeWith' $ OrganizationsForumsBoardsThreads organizationResponseName forumResponseName boardResponseName (EditS threadResponseName))
        --     (button_deleteThread $ routeWith' $ OrganizationsForumsBoardsThreads organizationResponseName forumResponseName boardResponseName (DeleteS threadResponseName))
        --     permExecuteEmpty
      elemText "TODO FIXME"

      plumbing_posts



viewNew
  :: Loader (Maybe BoardPackResponse)
  -> Maybe ThreadRequest
  -> HTMLView_

viewNew !l_m_board !m_request = do
  Loader.maybeLoader1 l_m_board $ \BoardPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyCreate boardPackResponseBoardId Nothing request



viewEditS
  :: Loader (Maybe ThreadPackResponse)
  -> Maybe ThreadRequest
  -> HTMLView_

viewEditS !l_m_thread !m_request =
  Loader.maybeLoader1 l_m_thread $ \ThreadPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyUpdate (threadResponseBoardId threadPackResponseThread) (Just threadPackResponseThreadId) request



viewMod
  :: TyCRUD
  -> BoardId
  -> Maybe ThreadId
  -> ThreadRequest
  -> HTMLView_

viewMod !tycrud' !board_id' !m_thread_id' !request' = do
  defineViewWithSKey "threads-mod" (tycrud', board_id', m_thread_id', request') go
  where
  go (tycrud, board_id, m_thread_id, request) = do

    let
      ThreadRequest{..} = request

    div_ $ do
      h1_ $ elemText $ linkName tycrud <> " Thread"

      mandatoryNameField threadRequestDisplayName (dispatch . Thread.setDisplayName request)

      renderedText "Safe name: " (toSafeUrl threadRequestDisplayName)

      optionalDescriptionField threadRequestDescription
        (dispatch . Thread.setDescription request)
        (dispatch $ Thread.clearDescription request)

      mandatoryBooleanYesNoField "Sticky" threadRequestSticky False
        (dispatch . Thread.setSticky request)

      mandatoryBooleanYesNoField "Locked" threadRequestLocked False
        (dispatch . Thread.setLocked request)

      p_ $ elemText "poll: TODO"

      tagsField
        threadRequestTags
        (maybe "" id threadRequestStateTag)
        (dispatch . Thread.setTag request)
        (dispatch $ Thread.addTag request)
        (dispatch . Thread.deleteTag request)
        (dispatch $ Thread.clearTags request)

      createButtonsCreateEditCancel
        m_thread_id
        (dispatch Save)
        (const $ dispatch Save)
        (routeWith' Home)
