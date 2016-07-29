{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Organizations (
  Store,
  defaultStore,
  Action (..),
  store,
  view_,
  view,
  viewIndex,
  viewIndex_,
  viewIndex__
) where



import           Control.Concurrent                   (forkIO)
import           Control.DeepSeq                      (NFData)
import           Control.Monad                        (void)
import           Control.Monad.Trans.Either           (EitherT, runEitherT)
import           Data.Ebyam                           (ebyam)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Monoid                          ((<>))
import           Data.Rehtie                          (rehtie)
import           Data.Text                            (Text)
import           Data.Typeable                        (Typeable)
import           GHC.Generics                         (Generic)
import           Haskell.Helpers.Either               (mustPassT)
import           React.Flux                           hiding (view)
import qualified React.Flux                           as RF
import qualified Web.Bootstrap3                       as B

import           LN.Api                               (getForumPacks_ByOrganizationId',
                                                       getOrganizationPacks,
                                                       getOrganizationsCount',
                                                       postOrganization',
                                                       putOrganization')
import           LN.Api.String                        (getOrganizationPack')
import           LN.Generate.Default                  (defaultOrganizationRequest)
import           LN.T.Convert                         (organizationResponseToOrganizationRequest)
import           LN.T.Organization                    (OrganizationRequest (..), OrganizationResponse (..), OrganizationResponse (..))
import           LN.T.Pack.Forum                      (ForumPackResponse (..),
                                                       ForumPackResponses (..))
import           LN.T.Pack.Organization               (OrganizationPackResponse (..), OrganizationPackResponses (..))
import           LN.T.Size                            (Size (..))
import           LN.T.User                            (UserSanitizedResponse (..))
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
import           LN.UI.ReactFlux.Access
import qualified LN.UI.ReactFlux.App.Delete           as Delete
import qualified LN.UI.ReactFlux.App.Forums           as Forums (viewIndex_)
import qualified LN.UI.ReactFlux.App.Gravatar         as Gravatar
import           LN.UI.ReactFlux.App.Loader           (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader           as Loader
import qualified LN.UI.ReactFlux.App.NotFound         as NotFound (view_)
import           LN.UI.ReactFlux.App.PageNumbers      (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers      as PageNumbers
import qualified LN.UI.ReactFlux.App.Route            as Route
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahref, ahrefName,
                                                       className_, classNames_)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.View.Button
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal        (showTagsSmall)



data Store = Store {
  _m_requestTag    :: Maybe Text
}



data Action
  = Save
  | Edit
  | Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st@Store{..} = do

    case action of
      Nop  -> pure st
      Save -> action_save
      Edit -> action_edit

    where

    action_save = pure st

    action_edit = pure st

    -- action_set_request request =
    --   pure $ st{
    --     _m_request = Just request
    --   }

    -- action_set_request_state m_req m_tag = pure $ st{ _m_request = m_req, _m_requestTag = m_tag }

    -- action_save = do
    --   case _m_request of
    --     Nothing                   -> pure st
    --     Just organization_request -> do
    --       lr <- rd $ postOrganization' $ organization_request { organizationRequestEmail = _requestEmail }
    --       rehtie lr (const $ pure st) $ \organization_response@OrganizationResponse{..} -> do
    --         void $ forkIO $ executeAction $ SomeStoreAction Route.store $ Route.Goto $ routeWith (Organizations (ShowS $ organizationResponseName)) []
    --         pure st

    -- action_edit edit_id = do
    --   case _m_request of
    --     Nothing                   -> pure st
    --     Just organization_request -> do
    --       lr <- rd $ putOrganization' edit_id $ organization_request { organizationRequestEmail = _requestEmail }
    --       rehtie lr (const $ pure st) $ \organization_response@OrganizationResponse{..} -> do
    --         void $ forkIO $ executeAction $ SomeStoreAction Route.store $ Route.Goto $ routeWith (Organizations (ShowS $ organizationResponseName)) []
    --         pure st



sync :: Store -> Text -> IO Store
sync st@Store{..} org_sid = do
  pure st



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store {
  _m_requestTag    = Nothing
}



view_ :: CRUD -> HTMLEvent_
view_ crud =
  RF.view view crud mempty



view :: ReactView CRUD
view = defineControllerView "organizations" store $ \st@Store{..} crud ->
  case crud of
    Index           -> viewIndex st
    -- ShowS org_sid   -> viewShowS _lm_organization _l_forums
    -- New             -> viewNew _m_requestTag _m_request
    -- EditS org_sid   -> viewEditS _m_requestTag _m_request _lm_organization
    -- DeleteS org_sid -> Delete.view_
    -- _               -> NotFound.view_



viewIndex :: Store -> HTMLView_
viewIndex Store{..} = mempty



viewIndex_ :: PageInfo -> Loader (Map OrganizationId OrganizationPackResponse) -> HTMLView_
viewIndex_ page_info l_organizations = do
  Loader.loader1 l_organizations (viewIndex__ page_info)



viewIndex__ :: PageInfo -> Map OrganizationId OrganizationPackResponse -> HTMLView_
viewIndex__ page_info organizations = do
  cldiv_ B.containerFluid $ do
    h1_ "Organizations"
    ahref $ routeWith' $ Organizations New
    PageNumbers.view_ (page_info, routeWith' $ Organizations Index)
    ul_ [className_ B.listUnstyled] $ do
      mapM_ (\OrganizationPackResponse{..} -> do
        let OrganizationResponse{..} = organizationPackResponseOrganization
        li_ $ do
          cldiv_ B.row $ do
            cldiv_ B.colXs1 $ p_ $ Gravatar.viewUser_ XSmall organizationPackResponseUser
            cldiv_ B.colXs3 $ p_ $ ahrefName organizationResponseDisplayName (routeWith' $ Organizations (ShowS organizationResponseName))
            cldiv_ B.colXs6 $ p_ $ elemText $ maybe "No Description." id organizationResponseDescription
            cldiv_ B.colXs2 $ p_ $ elemText $ prettyUTCTimeMaybe organizationResponseCreatedAt
        ) organizations



viewShowS :: Loader (Maybe OrganizationPackResponse) -> Loader (Map OrganizationId ForumPackResponse) -> HTMLView_
viewShowS lm_organization l_forums = do
  Loader.loader2 lm_organization l_forums $ go
  where
  go (Just organization_pack@OrganizationPackResponse{..}) forums_map = do
    let OrganizationResponse{..} = organizationPackResponseOrganization
    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ do
        h1_ [className_ B.textCenter] $ elemText $ organizationResponseDisplayName
        p_ [className_ B.textCenter] $ elemText $ maybe "" id organizationResponseDescription

        -- ACCESS: Organization
        -- * Member: if not a member, this is a shortcut to join an organization
        ---
        isMemberOfOrganizationHTML
         organization_pack
         mempty
         (button_joinOrganization $ routeWith' $ OrganizationsMembership organizationResponseName Index)

        -- ACCESS: Organization
        -- * Update: can edit organization settings
        -- * Delete: can delete organization
        --
        permissionsHTML'
          organizationPackResponsePermissions
          permCreateEmpty
          permReadEmpty
          (button_editOrganization $ routeWith' $ Organizations (EditS organizationResponseName))
          (button_deleteOrganization $ routeWith' $ Organizations (DeleteS organizationResponseName))
          permExecuteEmpty

    cldiv_ B.pageHeader $ do
      p_ $ h4_ $ do
        elemText "Name:"
        small_ $ elemText (" " <> organizationResponseName)

      ebyam organizationResponseDescription mempty $ \desc -> do
        p_ $ do
          h4_ $ do
            elemText "Description"
            small_ $ elemText desc

      p_ $ h4_ $ do
        elemText "Company"
        small_ $ elemText $ " " <> organizationResponseCompany

      p_ $ h4_ $ do
        elemText "Location"
        small_ $ elemText $ " " <> organizationResponseLocation

      p_ $ h4_ $ do
        elemText "Location"
        small_ $ elemText $ " " <> tshow organizationResponseMembership

      p_ $ h4_ $ do
        elemText "Location"
        small_ $ elemText $ " " <> tshow organizationResponseVisibility

      p_ $ h4_ $ do
        elemText "Tags: "
        showTagsSmall organizationResponseTags

    Forums.viewIndex_ organization_pack forums_map
    -- renderView_Forums_Index' org_pack forum_packs,
    -- p_ $ ahref (OrganizationMembership organizationResponseName Index emptyParams)
    -- p_ $ ahref (OrganizationTeams organizationResponseName Index emptyParams)
  go _ _ = NotFound.view_




viewNew :: Maybe Text -> Maybe OrganizationRequest -> HTMLView_
viewNew m_tag m_request =
  ebyam m_request mempty $ \request -> viewMod TyCreate Nothing m_tag request



viewEditS :: Maybe Text -> Maybe OrganizationRequest -> Loader (Maybe OrganizationPackResponse) -> HTMLView_
viewEditS m_tag m_request l_organization_pack =
  Loader.loader1 l_organization_pack $ \m_organization_pack -> do
    case (m_request, m_organization_pack) of
      (Just request, Just OrganizationPackResponse{..}) -> viewMod TyUpdate (Just organizationPackResponseOrganizationId) m_tag request
      (_, _) -> mempty



viewMod :: TyCRUD -> Maybe OrganizationId -> Maybe Text -> OrganizationRequest -> HTMLView_
viewMod tycrud m_organization_id m_tag request@OrganizationRequest{..} = do
  mempty
  -- div_ $ do
  --   h1_ $ elemText $ linkName tycrud <> " Organization"

  --   mandatoryNameField organizationRequestDisplayName
  --     (\input -> dispatch $ SetRequest $ request{organizationRequestDisplayName = input})

  --   optionalDescriptionField organizationRequestDescription
  --     (\input -> dispatch $ SetRequest $ request{organizationRequestDescription = Just input})
  --     (dispatch $ SetRequest $ request{organizationRequestDescription = Nothing})

  --   mandatoryCompanyField organizationRequestCompany
  --     (\input -> dispatch $ SetRequest $ request{organizationRequestCompany = input})

  --   mandatoryLocationField organizationRequestLocation
  --     (\input -> dispatch $ SetRequest $ request{organizationRequestLocation = input})

  --   mandatoryMembershipField organizationRequestMembership
  --     (\input -> dispatch $ SetRequest $ request{organizationRequestMembership = input})

  --   mandatoryVisibilityField organizationRequestVisibility
  --     (\input -> dispatch $ SetRequest $ request{organizationRequestVisibility = input})

  --   -- icon

  --   tagsField
  --      organizationRequestTags
  --      (maybe ""  id m_tag)
  --      (\input -> dispatch $ SetRequestState (Just request) (Just input))
  --      (dispatch $ SetRequestState (Just $ request{organizationRequestTags = maybe organizationRequestTags (\tag -> organizationRequestTags <> [tag]) m_tag}) Nothing)
  --      (\idx -> dispatch $ SetRequest $ request{organizationRequestTags = deleteNth idx organizationRequestTags})
  --      (dispatch $ SetRequest $ request{organizationRequestTags = []})

  --   createButtonsCreateEditCancel
  --     m_organization_id
  --     (dispatch Save)
  --     (\edit_id -> dispatch $ Edit edit_id)
  --     (routeWith' Home)



dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]