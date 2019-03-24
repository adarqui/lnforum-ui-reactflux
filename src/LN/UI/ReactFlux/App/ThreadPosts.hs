{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.ThreadPosts (
    viewIndex
  , viewIndex_
  , viewNew
  , viewEditI
  , viewShowI
  , viewPostData
  , postDataToBody
) where


import qualified Debug.Trace                           as Trace

import           Control.Concurrent                    (forkIO)
import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (forM_, void)
import           Control.Monad.Trans.Either            (EitherT, runEitherT)
import           Data.Int                              (Int64)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Monoid                           ((<>))
import           Data.Rehtie                           (rehtie)
import           Data.Text                             (Text)
import           Data.Tuple.Select
import           Data.Typeable                         (Typeable)
import           GHC.Generics                          (Generic)
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF

import           Data.BBCode                           hiding (linkName)
import           Data.BBCode.HTML.ReactFlux
import           Data.Ebyam                            (ebyam)
import           Haskell.Helpers.Either                (mustPassT)
import           LN.Api
import qualified LN.Api.String                         as ApiS
import           LN.Generate.Default                   (defaultBoardRequest)
import           LN.T.Board
import           LN.T.Convert
import           LN.T.Ent
import           LN.T.Forum
import           LN.T.Pack.Board
import           LN.T.Pack.Forum
import           LN.T.Pack.Sanitized.User
import           LN.T.Pack.Thread
import           LN.T.Pack.ThreadPost
import           LN.T.Param
import           LN.T.Profile
import           LN.T.Size
import           LN.T.Thread
import           LN.T.ThreadPost
import           LN.T.User
import           LN.UI.Core.Access
import qualified LN.UI.Core.App.ThreadPost             as ThreadPost
import           LN.UI.Core.Helpers.DataList           (deleteNth)
import           LN.UI.Core.Helpers.DataText           (tshow)
import           LN.UI.Core.Helpers.DataTime           (prettyUTCTimeMaybe)
import           LN.UI.Core.Helpers.GHCJS
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
import qualified LN.UI.ReactFlux.App.Like              as Like
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loader
import qualified LN.UI.ReactFlux.App.NotFound          as NotFound
import qualified LN.UI.ReactFlux.App.Oops              as Oops
import           LN.UI.ReactFlux.App.PageNumbers       (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers       as PageNumbers
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM  (ahref, ahrefClasses,
                                                        ahrefClassesName,
                                                        ahrefName, className_,
                                                        classNames_,
                                                        targetValue)
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.View.Button
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal
import qualified Web.Bootstrap3                        as B



viewIndex
  :: PageInfo
  -> UserId
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Maybe ThreadPackResponse)
  -> Loader (Map ThreadPostId ThreadPostPackResponse)
  -> Maybe ThreadPostRequest
  -> Map UserId UserSanitizedPackResponse
  -> HTMLView_

viewIndex !page_info' !me_id' !l_m_board' !l_m_thread' !l_posts' !m_request' !users_map' = do
  defineViewWithSKey "posts-index-1" (page_info', me_id', l_m_board', l_m_thread', l_posts', m_request', users_map') $ \(page_info, me_id, l_m_board, l_m_thread, l_posts, m_request, users_map) -> do

    h1_ [className_ B.textCenter] $ elemText "Posts"
    Loader.maybeLoader2 l_m_board l_m_thread $ \board thread -> do
      Loader.loader1 l_posts $ \posts -> do
        viewIndex_ page_info me_id board thread posts m_request users_map



viewIndex_
  :: PageInfo
  -> UserId
  -> BoardPackResponse
  -> ThreadPackResponse
  -> Map ThreadPostId ThreadPostPackResponse
  -> Maybe ThreadPostRequest
  -> Map UserId UserSanitizedPackResponse
  -> HTMLView_

viewIndex_ !page_info !me_id !board !thread !posts !m_request !users_map = do

  viewShared
    page_info
    me_id
    board
    thread
    posts
    m_request
    users_map



viewShowI
  :: PageInfo
  -> UserId
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Maybe ThreadPackResponse)
  -> Loader (Maybe ThreadPostPackResponse)
  -> Map UserId UserSanitizedPackResponse
  -> HTMLView_

viewShowI !page_info !me_id !l_m_board !l_m_thread !l_m_post !users_map = do
  Loader.maybeLoader3 l_m_board l_m_thread l_m_post $ \board thread post -> do
    viewShowI_
      page_info
      me_id
      board
      thread
      post
      users_map



viewShowI_
  :: PageInfo
  -> UserId
  -> BoardPackResponse
  -> ThreadPackResponse
  -> ThreadPostPackResponse
  -> Map UserId UserSanitizedPackResponse
  -> HTMLView_

viewShowI_ !page_info !me_id !board !thread !post !users_map = do
  case Map.lookup (userSanitizedResponseId $ threadPostPackResponseUser post) users_map of
    Nothing    -> p_ $ elemText "User not found in Map."
    Just !user -> viewShowI__ page_info me_id board thread post user



viewShowI__
  :: PageInfo
  -> UserId
  -> BoardPackResponse
  -> ThreadPackResponse
  -> ThreadPostPackResponse
  -> UserSanitizedPackResponse
  -> HTMLView_

viewShowI__ !page_info !me_id !board !thread !post !user = do

  let
    BoardPackResponse{..}         = board
    BoardResponse{..}             = boardPackResponseBoard
    ThreadPackResponse{..}        = thread
    ThreadResponse{..}            = threadPackResponseThread
    ThreadPostPackResponse{..}    = post
    ThreadPostResponse{..}        = threadPostPackResponseThreadPost
    UserSanitizedPackResponse{..} = user
    UserSanitizedResponse{..}     = userSanitizedPackResponseUser
    ProfileResponse{..}           = userSanitizedPackResponseProfile

  li_ $ cldiv_ B.row $ do
    cldiv_ B.colXs2 $ do
      ahref $ routeWith' $ Users (ShowS userSanitizedResponseName)
      p_ $ Gravatar.viewUser Medium threadPostPackResponseUser
      viewUserStats user
    cldiv_ B.colXs7 $ do
      ahrefName (threadResponseName <> "/" <> tshow threadPostResponseId) $ routeWith' $ BoardsThreadsPosts boardResponseName threadResponseName (ShowI threadPostResponseId)
      p_ $ elemText (prettyUTCTimeMaybe threadPostResponseCreatedAt)
      button_ [onClick $ \_ _ -> dispatch $ ThreadPost.quote post] $ elemText "quote"

      viewPostData threadPostResponseBody

      cldiv_ B.pageHeader mempty
      p_ $ elemText $ maybe "" id profileResponseSignature

    cldiv_ B.colXs1 $ do
      -- ACCESS: ThreadPost
      -- * Update: edit thread post
      -- * Delete: delete thread post
      --
      cldiv_ B.row $ do
        permissionsHTML'
          threadPostPackResponsePermissions
          permCreateEmpty
          permReadEmpty
          (button_editThreadPost $ routeWith' $ BoardsThreadsPosts boardResponseName threadResponseName (EditI threadPostResponseId))
          (button_deleteThreadPost $ routeWith' $ BoardsThreadsPosts boardResponseName threadResponseName (DeleteI threadPostResponseId))
          permExecuteEmpty

      -- ACCESS: Member & Not self
      -- Member: must be a member to like/star
      -- Not Self: can't like/star your own posts
      Like.view Ent_ThreadPost threadPostResponseId threadPostPackResponseLike

    -- TODO FIXME: STOPPED WORKING
    cldiv_ B.colXs2 $ do
      -- works: viewPostStats $ ThreadPostStatResponse 0 0 0 0 0 0
      -- viewPostStats $ ThreadPostStatResponse 0 0 0 0 0 0
      viewPostStats threadPostPackResponseStat
      -- p_ $ elemShow threadPostPackResponseStat
      p_ $ elemText "TODO FIXME"



viewShared
  :: PageInfo
  -> UserId
  -> BoardPackResponse
  -> ThreadPackResponse
  -> Map ThreadPostId ThreadPostPackResponse
  -> Maybe ThreadPostRequest
  -> Map UserId UserSanitizedPackResponse
  -> HTMLView_

viewShared
  !page_info'
  !me_id'
  !board'
  !thread'
  !posts'
  !m_request'
  !users_map'
  =
  defineViewWithSKey "posts-shared" (page_info', me_id', board', thread', posts', m_request', users_map') go

  where
  go (page_info, me_id, board, thread, posts, m_request, users_map) = do

    let
      BoardPackResponse{..}        = board
      BoardResponse{..}            = boardPackResponseBoard
      ThreadPackResponse{..}       = thread
      ThreadResponse{..}           = threadPackResponseThread

    div_ $ do
      PageNumbers.view1 page_info (routeWith' $ BoardsThreads boardResponseName (ShowS threadResponseName))
      ul_ ["key" $= "posts-list", className_ B.listUnstyled] $ do

        forM_ (Map.elems posts) $ \post -> do

          -- doesn't re-render:
          -- iframe_ [ "src" $= "https://www.youtube.com/embed/AVWRQ21Iorc", "height" @= (40 :: Int), "width" @= (100 :: Int) ] mempty

          -- re-renders:
          viewShowI_ page_info me_id board thread post users_map

          -- doesn't re-render!!!!
          -- case parseBBCodeWith (defaultParseReader { allowNotClosed = True }) (postDataToBody $ threadPostResponseBody $ threadPostPackResponseThreadPost post) of
          --      Left err    -> p_ $ elemText $ "error: " <> err
          --      Right codes -> p_ $ runBBCodeToHTML codes

          -- doesn't re-render:
          -- iframe_ [ "src" $= "https://www.youtube.com/embed/AVWRQ21Iorc", "height" @= (40 :: Int), "width" @= (100 :: Int) ] mempty

        -- INPUT FORM AT THE BOTTOM
        -- ACCESS: Thread
        -- * Create: post within a thread
        --
        ebyam m_request mempty $ \request -> do
          permissionsMatchCreateHTML
            threadPackResponsePermissions
            (viewMod TyCreate threadResponseId Nothing request)
            mempty
      PageNumbers.view2 page_info (routeWith' $ BoardsThreads boardResponseName (ShowS threadResponseName))



viewNew
  :: Loader (Maybe ThreadPackResponse)
  -> Maybe ThreadPostRequest
  -> HTMLView_

viewNew !l_m_thread !m_request = do
  Loader.maybeLoader1 l_m_thread $ \ThreadPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyCreate threadPackResponseThreadId Nothing request



viewEditI
  :: Loader (Maybe ThreadPostPackResponse)
  -> Maybe ThreadPostRequest
  -> HTMLView_

viewEditI !l_m_post !m_request =
  Loader.maybeLoader1 l_m_post $ \ThreadPostPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyUpdate (threadPostResponseThreadId threadPostPackResponseThreadPost) (Just threadPostPackResponseThreadPostId) request



viewMod
  :: TyCRUD
  -> ThreadId
  -> Maybe ThreadPostId
  -> ThreadPostRequest
  -> HTMLView_

viewMod !tycrud' !thread_id' !m_post_id' !request' = do
  defineViewWithSKey "posts-mod" (tycrud', thread_id', m_post_id', request') go
  where
  go (tycrud, thread_id, m_post_id, request) = do

    let
      ThreadPostRequest{..} = request
      body                  = postDataToBody threadPostRequestBody

    div_ $ do
      h1_ $ elemText $ linkName tycrud <> " Post"

      cldiv_ B.row $ do
        cldiv_ B.colXs2 mempty
        cldiv_ B.colXs9 $ do
          tagsField
            threadPostRequestTags
            (maybe "" id threadPostRequestStateTag)
            (dispatch . ThreadPost.setTag request)
            (dispatch $ ThreadPost.addTag request)
            (dispatch . ThreadPost.deleteTag request)
            (dispatch $ ThreadPost.clearTags request)

          cldiv_ B.well $ do
            textarea_ [ className_ B.formControl
                      , "rows" $= "10"
                      , "value" @= body
                      , onChange $ \input -> dispatch $ ThreadPost.setBody request (PostDataBBCode $ targetValue input)
                      ] mempty
            -- TODO FIXME: cancel
            case tycrud of
              TyCreate -> do
                button_ [onClick $ \_ _ -> dispatch SaveThreadPostInPlace] $ elemText "send"
              TyUpdate -> do
                button_ [onClick $ \_ _ -> dispatch $ Goto $ routeWith' Home ] $ elemText "cancel"
                button_ [onClick $ \_ _ -> dispatch SaveThreadPost] $ elemText "send"
        cldiv_ B.colXs1 mempty



viewUserStats
  :: UserSanitizedPackResponse
  -> HTMLView_

viewUserStats !user =
  showTableClean
    []
    ["respect", "threads", "posts", "workouts", "resources", "leurons"]
    [[userSanitizedStatResponseRespect]
    ,[userSanitizedStatResponseThreads]
    ,[userSanitizedStatResponseThreadPosts]
    ]
  where
  UserSanitizedPackResponse{..} = user
  UserSanitizedStatResponse{..} = userSanitizedPackResponseStat



viewPostStats
  :: ThreadPostStatResponse
  -> HTMLView_

viewPostStats !stat =
  showTableClean
    []
    ["score", "up", "neutral", "down", "stars", "views"]
    [[0]
    ,[threadPostStatResponseLikes]
    ,[0]
    ,[0]
    ,[0]
    ,[0]
    ]
  where
  ThreadPostStatResponse{..} = stat

-- viewPostStats !stat =
--   showTableClean
--     []
--     ["score", "up", "neutral", "down", "stars", "views"]
--     [[threadPostStatResponseLikes - threadPostStatResponseDislikes]
--     ,[threadPostStatResponseLikes]
--     ,[threadPostStatResponseNeutral]
--     ,[threadPostStatResponseDislikes]
--     ,[threadPostStatResponseViews]
--     ]
--   where
--   ThreadPostStatResponse{..} = stat

-- viewPostStats stat =
--   do
--     p_ $ elemText "viewPostStats"
--     -- p_ $ elemShow (threadPostStatResponseLikes - threadPostStatResponseLikes)
--     -- p_ $ elemShow (threadPostStatResponseLikes stat)
--     -- p_ $ elemShow (threadPostStatResponseLikes stat - threadPostStatResponseViews stat)
--     -- p_ $ elemShow (threadPostStatResponseLikes - threadPostStatResponseDislikes)
--     -- p_ $ elemShow threadPostStatResponseLikes
--     -- p_ $ elemShow threadPostStatResponseLikes
--     -- p_ $ elemShow threadPostStatResponseNeutral
--     -- p_ $ elemShow threadPostStatResponseDislikes
--     -- p_ $ elemShow threadPostStatResponseViews
--     -- p_ $ elemShow (threadPostStatResponseLikes stat)
--     -- p_ $ elemShow (threadPostStatResponseNeutral stat)
--     -- p_ $ elemShow (threadPostStatResponseDislikes stat)
--     -- p_ $ elemShow (threadPostStatResponseViews stat)
--     -- p_ $ elemShow $ threadPostStatResponseLikes - threadPostStatResponseDislikes
--   -- where
--   -- ThreadPostStatResponse{..} = stat



viewPostData
  :: PostData
  -> HTMLView_

viewPostData !body = cldiv_ "thread-post-body" $
  case body of
    PostDataEmpty      -> p_ [className_ "post-data-empty"] mempty
    PostDataRaw v      -> p_ [className_ "post-data-raw"] $ elemText v
    PostDataBBCode v   -> do
      case parseBBCodeWith bbcode_options v of
           Left err    -> p_ $ elemText $ "error: " <> err
           Right codes -> p_ $ runBBCodeToHTMLWith bbcode_options codes
      where
      bbcode_options = defaultParseReader {
                           allowNotClosed = True
                         , emoticons      = Just (defaultEmoticonsBimap, "/static/emoticons")
                         , linkResource   = Just id
                       }
    PostDataMarkdown v -> p_ [className_ "post-data-markdown"] $ elemText "markdown"
    _                  -> p_ [className_ "post-data-unknown"] $ elemText "unknown post body"



postDataToBody
  :: PostData
  -> Text

postDataToBody !p = case p of
  PostDataRaw v    -> v
  PostDataBBCode v -> v
  _                -> ""
