{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.Repository where
import Data.Monoid (mconcat)
import qualified Data.Text.Encoding as TE

import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)
import Control.Monad.Trans.Resource

import GitLab.Rest (restSource)
import GitLab.Types

listBranches
  :: (MonadResource m)
  => ProjectId
  -> Source (GitLabT m) Branch
listBranches projId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projId
      , "/repository/branches"
      ]
  }

listTags
  :: (MonadResource m)
  => ProjectId
  -> Source (GitLabT m) Tag
listTags projId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projId
      , "/repository/tags"
      ]
  }

listCommits
  :: (MonadResource m)
  => ProjectId
  -> Source (GitLabT m) RepositoryCommit
listCommits projId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projId
      , "/repository/commits"
      ]
  }

listRepositoryTree
   :: (MonadResource m)
  => ProjectId
  -> Source (GitLabT m) RepositoryTree
listRepositoryTree projId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projId
      , "/repository/tree"
      ]
  }
