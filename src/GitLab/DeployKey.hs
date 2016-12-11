{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.DeployKey where
import Data.Monoid (mconcat)
import qualified Data.Text.Encoding as TE

import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)
import Control.Monad.Trans.Resource

import GitLab.Rest (restSource)
import GitLab.Types

listDeployKeys
  :: (MonadResource m)
  => ProjectId
  -> Source (GitLabT m) DeployKey
listDeployKeys projId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projId
      , "/keys"
      ]
  }
