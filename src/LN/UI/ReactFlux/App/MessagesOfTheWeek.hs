{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFlux.App.MessagesOfTheWeek (
  viewMessagesOfTheWeek,
  viewMessagesOfTheWeek_
) where



import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           React.Flux                            hiding (view)
import qualified Web.Bootstrap3                        as B

import           LN.T.Pack.ThreadPost
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loading
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types



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
