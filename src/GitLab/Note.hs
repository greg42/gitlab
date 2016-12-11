{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.Note where
import Data.Monoid (mconcat)
import qualified Data.Text.Encoding as TE

import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)
import Control.Monad.Trans.Resource

import GitLab.Rest (restSource)
import GitLab.Types

listNotes
  :: (MonadResource m)
  => ProjectId
  -> Source (GitLabT m) Note
listNotes projId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projId
      , "/notes"
      ]
  }
