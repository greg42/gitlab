{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
#ifdef DEBUG
{-# LANGUAGE ScopedTypeVariables #-}
#endif
module GitLab.Rest
  ( rest
  , restSource

  , paginate
  , paginateBy
  ) where

import Control.Monad.Reader
import Data.Monoid ((<>))
import Data.Text (Text)

import Data.Aeson as A
import Data.Conduit
import Network.HTTP.Conduit
import Network.HTTP.Types
import Web.PathPieces (toPathPiece)
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import Data.Default (def)

import GitLab.Monad

#ifdef DEBUG
import Data.Attoparsec.Lazy (parse, Result(Done))
#endif

rest ::
#ifdef DEBUG
  forall m a.
#endif
  ( FromJSON a
  , MonadBaseControl IO m
  , MonadResource m
#ifdef DEBUG
  , Show a
#endif
  )
  => (Request -> Request)
  -> GitLabT m (Maybe a)
rest f = do
  GitLabConfig {..} <- ask
  request <- auth . modifyPath . f $ defaultRequest
    { secure = gitLabSecure
    , host = gitLabHost
    , port = gitLabPort
    }
  response <- httpLbs request gitLabManager
#ifdef DEBUG
  case parse A.json (responseBody response) of
    Done _ val -> liftIO $ print (fromJSON val :: A.Result a)
    _ -> return ()
#endif
  return $ A.decode $ responseBody response

restSource ::
#ifdef DEBUG
  forall m a.
#endif
  ( FromJSON a
  , MonadResource m
#ifdef DEBUG
  , Show a
#endif
  )
  => (Request -> Request)
  -> Source (GitLabT m) a
restSource f = loop 1
  where
    loop !page = do
      GitLabConfig {..} <- lift ask
      request <- lift . auth . modifyPath . f $ defaultRequest
        { method = "GET"
        , secure = gitLabSecure
        , host = gitLabHost
        , port = gitLabPort
        , queryString =
            renderQuery False $ paginationQuery page gitLabPagination
        }
      response <- lift $ httpLbs request gitLabManager
#ifdef DEBUG
      case parse A.json (responseBody response) of
        Done _ val ->
          case fromJSON val :: A.Result [a] of
            A.Error reason -> do
              liftIO $ print (responseBody response)
              liftIO $ print reason
            _ -> return ()
        _ -> return ()
#endif
      case A.decode' $ responseBody response of
        Nothing -> return ()
        Just entities -> do
          CL.sourceList entities
          let numEntities = length entities
          case gitLabPagination of
            NoPagination -> return ()
            PaginateBy n -> when (numEntities == n) $ loop $ page + 1

modifyPath :: Request -> Request
modifyPath request = request
  { path = "/api/v3" <> path request
  }

auth
  :: Monad m
  => Request 
  -> GitLabT m (Request)
auth request = do
  privToken <- asks (credsPrivateToken . gitLabCreds)
  let privateTokenHeader = ("PRIVATE-TOKEN", privToken)
  return request
    { requestHeaders = privateTokenHeader : requestHeaders request
    }

paginationQuery :: Int -> Pagination -> Query
paginationQuery page pagination = toQuery $ case pagination of
   NoPagination -> [] :: [(String, Text)]
   PaginateBy perPage ->
     [ ("page", toPathPiece (page `max` 1))
     , ("per_page", toPathPiece perPage)
     ]

paginate :: Monad m => GitLabT m a -> GitLabT m a
paginate = local $ \config -> config
  { gitLabPagination = defaultPagination
  }

paginateBy :: Monad m => Int -> GitLabT m a -> GitLabT m a
paginateBy perPage = local $ \config -> config
  { gitLabPagination = userPagination perPage
  }
