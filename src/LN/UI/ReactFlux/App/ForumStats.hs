{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFlux.App.ForumStats (
  viewForumStats,
  viewForumStats_
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
