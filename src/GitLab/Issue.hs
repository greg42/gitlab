{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.Issue where
import Data.Monoid (mconcat)
import qualified Data.Text.Encoding as TE

import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)
import Control.Monad.Trans.Resource

import GitLab.Rest (restSource)
import GitLab.Types

listIssues
  :: (MonadResource m)
  => Source (GitLabT m) Issue
listIssues = restSource $ \request -> request
  { path = "/issues"
  }

listProjectIssues
  :: (MonadResource m)
  => ProjectId
  -> Source (GitLabT m) Issue
listProjectIssues projId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projId
      , "/issues"
      ]
  }
