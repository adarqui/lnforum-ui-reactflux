{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFlux.App.UsersOnline (
  viewUsersOnline,
  viewUsersOnline_
) where



import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           React.Flux                            hiding (view)
import qualified Web.Bootstrap3                        as B

import           LN.T.User
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loading
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types



--
-- Users Online
-- 1 Guest, 1 User
-- Users active in past 15 minutes:
-- adarqui
-- Most Online Today: 4. Most Online Ever: 219 (September 14, 2012, 04:53:02 pm)
--
viewUsersOnline
  :: Loader (Map UserId UserSanitizedResponse)
  -> HTMLView_

viewUsersOnline l_users_online_map = do
  defineViewWithSKey "users-online-1" (l_users_online_map) $ \l_users_online_map' -> do
    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ do
        h4_ $ elemText "Users Online"
        Loading.loader1 l_users_online_map' $ \users_online_map -> do
          viewUsersOnline_ users_online_map

viewUsersOnline_
  :: Map UserId UserSanitizedResponse
  -> HTMLView_

viewUsersOnline_ users_online_map = do
  defineViewWithSKey "users-online-2" (users_online_map) go
  where
  go users_online_map' = do
    p_ $ elemText "TODO FIXME: users-online"
