{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Boards (
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
import           LN.T.Pack.Thread
import           LN.T.Pack.ThreadPost
import           LN.T.Param
import           LN.T.Size
import           LN.T.Thread
import           LN.T.ThreadPost
import           LN.T.User
import           LN.UI.Core.App.Board                  as Board
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
import           LN.UI.ReactFlux.App.Core.Shared
import qualified LN.UI.ReactFlux.App.Delete            as Delete
import qualified LN.UI.ReactFlux.App.Gravatar          as Gravatar
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loader
import qualified LN.UI.ReactFlux.App.NotFound          as NotFound
import qualified LN.UI.ReactFlux.App.Oops              as Oops
import           LN.UI.ReactFlux.App.PageNumbers       (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers       as PageNumbers
import qualified LN.UI.ReactFlux.App.Threads           as Threads
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
  -> Loader (Map BoardId BoardPackResponse)
  -> HTMLView_

viewIndex !page_info' !l_boards' = do
  defineViewWithSKey "boards-index-1" (page_info', l_boards') $ \(page_info, l_boards) -> do
    h1_ [className_ B.textCenter] $ elemText "Boards"
    Loader.loader1 l_boards $ \boards -> do
      viewIndex_ page_info boards



viewIndex_
  :: PageInfo
  -> Map BoardId BoardPackResponse
  -> HTMLView_

viewIndex_ !page_info' !boards_map' = do
  defineViewWithSKey "boards-index-2" (page_info', boards_map') $ \(page_info, boards_map) -> do
    ul_ [className_ B.listUnstyled] $
      forM_ (Map.elems boards_map) $ \BoardPackResponse{..} -> do
        let
          BoardResponse{..}     = boardPackResponseBoard
          BoardStatResponse{..} = boardPackResponseStat
          m_thread              = boardPackResponseLatestThread
          m_post                = boardPackResponseLatestThreadPost
          m_user                = boardPackResponseLatestThreadPostUser
        li_ $ do
          cldiv_ B.row $ do
            cldiv_ B.colXs1 $ do
              p_ $ elemText "icon"
            cldiv_ B.colXs5 $ do
              p_ $ ahrefName boardResponseDisplayName $ routeWith' $ Boards (ShowS boardResponseName)
              p_ $ elemText $ maybe "No description." id boardResponseDescription
            cldiv_ B.colXs2 $ do
              showTableClean
                []
                ["threads", "posts", "views"]
                [[boardStatResponseThreads]
                ,[boardStatResponseThreadPosts]
                ,[boardStatResponseViews]
                ]
            cldiv_ B.colXs3 $ do
              case (m_thread, m_post, m_user) of
                (Just ThreadResponse{..}, Just ThreadPostResponse{..}, Just user@UserSanitizedResponse{..}) -> do
                  div_ $ do
                    p_ $ do
                      Gravatar.viewUser XSmall user
                      elemText " Last post by "
                      ahref $ routeWith' (Users (ShowS userSanitizedResponseName))
                    p_ $ do
                      elemText "in "
                      ahref $ routeWith (BoardsThreads boardResponseName (ShowS threadResponseName)) [(ParamTag_Offset, Offset (-1))]
                    p_ $ elemText $ prettyUTCTimeMaybe threadPostResponseCreatedAt
                _ -> div_ $ p_ $ elemText "No posts."
            cldiv_ B.colXs1 $ do
              cldiv_ B.container $ do
                buttonGroup_VerticalSm1 $ do
                  -- ACCESS: Forum
                  -- * Create: can create boards
                  --
                  -- permissionsMatchCreateHTML
                    -- forumPackResponsePermissions
                    -- TODO FIXME: Child board
                    -- (button_newBoard $ routeWith' $ Boards New)
                    -- mempty

                  -- ACCESS: Board
                  -- * Update: can edit board settings
                  -- * Delete: can delete boads
                  --
                  permissionsHTML'
                    boardPackResponsePermissions
                    permCreateEmpty
                    permReadEmpty
                    (button_editBoard $ routeWith' $ Boards (EditS boardResponseName))
                    (button_deleteBoard $ routeWith' $ Boards (DeleteS boardResponseName))
                    permExecuteEmpty





viewShowS
  :: PageInfo
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Map ThreadId ThreadPackResponse)
  -> HTMLView_

viewShowS !page_info' !l_m_board' !l_threads' = do
  defineViewWithSKey "boards-show-1" (page_info', l_m_board', l_threads') $ \(page_info, l_m_board, l_threads) -> do
    Loader.loader2 l_m_board l_threads $ \m_board threads -> do
      case (m_board) of
        Just board ->
          viewShowS_
            page_info
            board
            (Threads.viewIndex_ page_info board threads)
        _ -> Oops.view



viewShowS_
  :: PageInfo
  -> BoardPackResponse
  -> HTMLView_ -- ^ plumbing threads
  -> HTMLView_

viewShowS_ !page_info' !board' !plumbing_threads' = do
  defineViewWithSKey "boards-show-2" (page_info', board', plumbing_threads') $ \(page_info, board, plumbing_threads) -> do

    let
      BoardPackResponse{..}        = board
      BoardResponse{..}            = boardPackResponseBoard

    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ do
        h2_ $ elemText boardResponseDisplayName
        p_ [className_ B.lead] $ elemText $ maybe "No description." id boardResponseDescription

        p_ $ do
          buttonGroup_HorizontalSm1 $ do
            -- ACCESS: Board
            -- * Create: can create sub-boards
            -- * Update: can edit board settings
            -- * Delete: can delete board
            --
            permissionsHTML'
              boardPackResponsePermissions
              (button_newBoard $ routeWith' $ Boards New)
              permReadEmpty
              (button_editBoard $ routeWith' $ Boards (EditS boardResponseName))
              (button_deleteBoard $ routeWith' $ Boards (DeleteS boardResponseName))
              permExecuteEmpty

        p_ $ do
          buttonGroup_HorizontalSm1 $ do
            -- ACCESS: Board
            -- * Create: can create threads
            --
            -- permissionsMatchCreateHTML
            --  boardPackResponseThreadPermissions
            --  (button_newThread $ routeWith' $ BoardsThreads boardResponseName New)
            --  mempty
            mempty

      div_ plumbing_threads



viewNew
  :: Loader (Maybe ForumPackResponse)
  -> Maybe BoardRequest
  -> HTMLView_

viewNew !l_m_forum !m_request = do
  Loader.maybeLoader1 l_m_forum $ \ForumPackResponse{..} ->
    mempty
    -- TODO FIXME:
    -- ebyam m_request mempty $ \request -> viewMod TyCreate forumPackResponseForumId Nothing request



viewEditS
  :: Loader (Maybe BoardPackResponse)
  -> Maybe BoardRequest
  -> HTMLView_
viewEditS !l_m_board !m_request =
  Loader.maybeLoader1 l_m_board $ \BoardPackResponse{..} ->
    mempty
    -- TODO FIXME:
    -- ebyam m_request mempty $ \request -> viewMod TyUpdate (boardResponseOrgId boardPackResponseBoard) (Just boardPackResponseBoardId) request



viewMod
  :: TyCRUD
  -> BoardRequest
  -> HTMLView_

viewMod !tycrud' !request' = do
  defineViewWithSKey "boards-mod" (tycrud', request') $ \(tycrud, request) -> do

    let
      BoardRequest{..} = request

    div_ $ do
      h1_ $ elemText $ linkName tycrud <> " Board"

      mandatoryNameField boardRequestDisplayName (dispatch . Board.setDisplayName request)

      renderedText "Safe name: " (toSafeUrl boardRequestDisplayName)

      optionalDescriptionField boardRequestDescription
        (dispatch . Board.setDescription request)
        (dispatch $ Board.clearDescription request)

      mandatoryBooleanYesNoField "Anonymous" boardRequestIsAnonymous False
        (dispatch . Board.setIsAnonymous request)

      mandatoryBooleanYesNoField "Can create sub-boards" boardRequestCanCreateBoards True
        (dispatch . Board.setCanCreateBoards request)

      mandatoryBooleanYesNoField "Can create threads" boardRequestCanCreateThreads True
        (dispatch . Board.setCanCreateThreads request)

      -- TODO FIXME: bring back - keep state local! not in the damn lnforum-type!
      -- tagsField
      --   boardRequestTags
      --   (maybe "" id boardRequestStateTag)
      --   (dispatch . Board.setTag request)
      --   (dispatch $ Board.addTag request)
      --   (dispatch . Board.deleteTag request)
      --   (dispatch $ Board.clearTags request)

      createButtonsCreateEditCancel
        Nothing -- TODO FIXME
        (dispatch Save)
        (const $ dispatch Save)
        (routeWith' Home)
