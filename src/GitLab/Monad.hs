{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module GitLab.Monad
  ( GitLabT, runGitLabT

  , GitLabConfig(..)
  , Credentials(..)

  , Pagination(..)
  , noPagination
  , defaultPagination
  , userPagination

  -- * Re-exports from http-conduit
  , HC.Manager
  , HC.withManager
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)

import Control.Monad.Base (MonadBase(..), liftBase)
import Control.Monad.Trans.Control
import qualified Network.HTTP.Conduit as HC

instance (MonadBaseControl IO m) => MonadBase IO (GitLabT m) where
     liftBase = GitLabT . liftBase

newtype GitLabT m a = GitLabT { unGitLabT :: ReaderT GitLabConfig m a }
  deriving
    ( Functor, Applicative, Monad, MonadIO
    , MonadReader GitLabConfig
    ,  MonadThrow
    , MonadTrans
    )

instance MonadBase b m => MonadBase b (GitLabT m) where
  liftBase = lift . liftBase

instance MonadTransControl GitLabT where
  type StT GitLabT a = StT (ReaderT GitLabConfig) a
  liftWith f = GitLabT $ liftWith (\run -> f (run . unGitLabT))
  restoreT = GitLabT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (GitLabT m) where
  type StM (GitLabT m) a = ComposeSt GitLabT m a
  liftBaseWith = defaultLiftBaseWith 
  restoreM = defaultRestoreM 

-- | Configurations to access to GitLab API
data GitLabConfig = GitLabConfig
  { gitLabCreds :: Credentials
  , gitLabManager :: HC.Manager -- ^ HTTP client manager
  , gitLabSecure :: Bool -- ^ HTTPS or not
  , gitLabHost :: ByteString -- ^ Host name of GitLab instance
  , gitLabPort :: Int -- ^ Listening port for GitLab instance
  , gitLabPagination :: Pagination -- ^ Pagination mode
  }

-- | Pagination mode
data Pagination
  = NoPagination -- ^ Fetch all result at once
  | PaginateBy Int -- ^ Fetch specified number of entries at once
  deriving (Eq, Ord)

noPagination :: Pagination
noPagination = NoPagination

defaultPagination :: Pagination
defaultPagination = PaginateBy 20

userPagination :: Int -> Pagination
userPagination = PaginateBy

data Credentials = Credentials
  { credsPrivateToken :: ByteString -- ^ User's private token
  }

-- | The only way to run @GitLabT m a@ action
runGitLabT :: GitLabConfig -> GitLabT m a -> m a
runGitLabT config (GitLabT m) = runReaderT m config
