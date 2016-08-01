{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
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



import           Control.Concurrent                   (forkIO)
import           Control.DeepSeq                      (NFData)
import           Control.Monad                        (void)
import           Control.Monad.Trans.Either           (EitherT, runEitherT)
import           Data.Ebyam                           (ebyam)
import           Data.Int                             (Int64)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Monoid                          ((<>))
import           Data.Rehtie                          (rehtie)
import           Data.Text                            (Text)
import           Data.Tuple.Select
import           Data.Typeable                        (Typeable)
import           GHC.Generics                         (Generic)
import           Haskell.Helpers.Either               (mustPassT)
import           React.Flux                           hiding (view)
import qualified React.Flux                           as RF
import qualified Web.Bootstrap3                       as B

import           LN.Api
import qualified LN.Api.String                        as ApiS
import           LN.Generate.Default                  (defaultBoardRequest)
import           LN.T.Board
import           LN.T.Convert
import           LN.T.Forum
import           LN.T.Organization
import           LN.T.Pack.Board
import           LN.T.Pack.Forum
import           LN.T.Pack.Organization
import           LN.T.Pack.Thread
import           LN.T.Pack.ThreadPost
import           LN.T.Param
import           LN.T.Size
import           LN.T.Thread
import           LN.T.ThreadPost
import           LN.T.User
import           LN.UI.Core.Helpers.DataList          (deleteNth)
import           LN.UI.Core.Helpers.DataText          (tshow)
import           LN.UI.Core.Helpers.DataTime          (prettyUTCTimeMaybe)
import           LN.UI.Core.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Core.Helpers.Map               (idmapFrom)
import           LN.UI.Core.PageInfo                  (PageInfo (..),
                                                       defaultPageInfo,
                                                       pageInfoFromParams,
                                                       paramsFromPageInfo)
import           LN.UI.Core.Router                    (CRUD (..), Params,
                                                       Route (..),
                                                       RouteWith (..),
                                                       TyCRUD (..), emptyParams,
                                                       linkName, routeWith,
                                                       routeWith')
import           LN.UI.Core.Sort
import           LN.UI.ReactFlux.Access
import qualified LN.UI.ReactFlux.App.Delete           as Delete
import qualified LN.UI.ReactFlux.App.Gravatar         as Gravatar
import           LN.UI.ReactFlux.App.Loader           (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader           as Loader
import qualified LN.UI.ReactFlux.App.NotFound         as NotFound (view_)
import qualified LN.UI.ReactFlux.App.Oops             as Oops (view_)
import           LN.UI.ReactFlux.App.PageNumbers      (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers      as PageNumbers
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahref, ahrefClasses,
                                                       ahrefClassesName,
                                                       ahrefName, className_,
                                                       classNames_)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.View.Button
import           LN.UI.ReactFlux.View.Button          (showBadge)
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal        (showTagsSmall)




viewIndex
  :: PageInfo
  -> Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Map ThreadId ThreadPackResponse)
  -> HTMLView_

viewIndex page_info l_m_organization l_m_forum l_m_board l_threads = do
  h1_ [className_ B.textCenter] $ elemText "Threads"
  Loader.loader1_ l_m_organization $ \organization -> do
    Loader.loader1_ l_m_forum $ \forum -> do
      Loader.loader1_ l_m_board $ \board -> do
        Loader.loader1 l_threads $ \threads -> do
          viewIndex_ organization forum board threads



viewIndex_
  :: OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> Map ThreadId ThreadPackResponse
  -> HTMLView_

viewIndex_ organization forum board threads = do
  p_ $ elemText "..."



viewShowS
  :: Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Map ThreadId ThreadPackResponse)
  -> HTMLView_

viewShowS lm_organization lm_forum lm_board l_threads = do
  Loader.loader4 lm_organization lm_forum lm_board l_threads $ \m_organization m_forum m_board threads -> do
    case (m_organization, m_forum, m_board) of
      (Just organization, Just forum, Just board) ->
        viewShowS_
          organization
          forum
          board
          mempty
      _ -> Oops.view_



viewShowS_
  :: OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> HTMLView_ -- ^ plumbing threads
  -> HTMLView_

viewShowS_ organization@OrganizationPackResponse{..} forum@ForumPackResponse{..} board@BoardPackResponse{..} plumbing_threads = do
  cldiv_ B.containerFluid $ do
    cldiv_ B.pageHeader $ do
      p_ [className_ B.lead] $ elemText $ maybe "No description." id boardResponseDescription

      div_ plumbing_threads

  where
  OrganizationResponse{..} = organizationPackResponseOrganization
  ForumResponse{..}        = forumPackResponseForum
  BoardResponse{..}        = boardPackResponseBoard



viewNew
  :: Loader (Maybe ForumPackResponse)
  -> Maybe Text
  -> Maybe BoardRequest
  -> HTMLView_
viewNew lm_forum m_tag m_request = do
  Loader.loader1_ lm_forum $ \ForumPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewNew_ forumPackResponseForumId m_tag request



viewNew_
  :: ForumId
  -> Maybe Text
  -> BoardRequest
  -> HTMLView_
viewNew_ forum_id m_tag request = viewMod TyCreate forum_id Nothing m_tag request



viewEditS
  :: Loader (Maybe BoardPackResponse)
  -> Maybe Text
  -> Maybe BoardRequest
  -> HTMLView_
viewEditS lm_board m_tag m_request =
  Loader.loader1_ lm_board $ \BoardPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyUpdate (boardResponseOrgId boardPackResponseBoard) (Just boardPackResponseBoardId) m_tag request



viewEditS_
  :: OrganizationId
  -> BoardId
  -> Maybe Text
  -> Maybe BoardRequest
  -> HTMLView_
viewEditS_ organization_id board_id m_tag m_request =
  ebyam m_request mempty $ \request -> viewMod TyUpdate organization_id (Just board_id) m_tag request



viewMod :: TyCRUD -> OrganizationId -> Maybe ForumId -> Maybe Text -> BoardRequest -> HTMLView_
viewMod tycrud organization_id m_forum_id m_tag request@BoardRequest{..} = do
  div_ $ do
    h1_ $ elemText $ linkName tycrud <> " Board"
