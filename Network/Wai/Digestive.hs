{-# LANGUAGE CPP #-}
-- | Helpers to bind 'digestive-functors' onto a 'wai' request
module Network.Wai.Digestive (queryFormEnv, bodyFormEnv, bodyFormEnv_, requestFormEnv, requestFormEnv_) where

import Control.Arrow (second)
#if MIN_VERSION_wai(2,0,0)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, getInternalState)
#else
import Control.Monad.Trans.Resource (ResourceT)
#endif
import Text.Digestive (Env, FormInput(..))
import Network.HTTP.Types.QueryLike (QueryLike, toQuery, toQueryValue)
import Network.Wai (Request, queryString)
import Network.Wai.Util (queryLookupAll)
import Network.Wai.Parse (parseRequestBody, BackEnd, File, fileContent, tempFileBackEnd)
import Data.Text (Text)
import qualified Data.Text as T

newtype FileQuery = FileQuery [File FilePath]

instance QueryLike FileQuery where
	toQuery (FileQuery files) =
		map (second (toQueryValue . fileContent)) files

-- Manual currying for performance
-- | Build an 'Text.Digestive.Types.Env' from a query
queryFormEnv :: (QueryLike q, Monad m) => q -> Env m
queryFormEnv qs = \pth ->
	return $ map TextInput $ queryLookupAll (pathToText pth) qs'
	where
	qs' = toQuery qs

-- | Build an 'Text.Digestive.Types.Env' from a request body
#if MIN_VERSION_wai(2,0,0)
bodyFormEnv :: (Monad m, MonadIO io) => BackEnd FilePath -> Request -> io (Env m)
bodyFormEnv backend req = liftIO $ do
#else
bodyFormEnv :: (Monad m) => BackEnd FilePath -> Request -> ResourceT IO (Env m)
bodyFormEnv backend req = do
#endif
	(query, files) <- parseRequestBody backend req
	return $ queryFormEnv (toQuery query ++ toQuery (FileQuery files))

-- | Build an 'Text.Digestive.Types.Env' from a request body
--
-- Uses a default temporary file 'Network.Wai.Parse.BackEnd'
#if MIN_VERSION_wai(2,0,0)
bodyFormEnv_ :: (Monad m, MonadIO io) => Request -> ResourceT io (Env m)
bodyFormEnv_ req = do
	st <- getInternalState
	bodyFormEnv (tempFileBackEnd st) req
#else
bodyFormEnv_ :: (Monad m) => Request -> ResourceT IO (Env m)
bodyFormEnv_ = bodyFormEnv tempFileBackEnd
#endif

-- | Build an 'Text.Digestive.Types.Env' from request body and query string
#if MIN_VERSION_wai(2,0,0)
requestFormEnv :: (Monad m, MonadIO io) => BackEnd FilePath -> Request -> io (Env m)
requestFormEnv backend req = liftIO $ do
#else
requestFormEnv :: (Monad m) => BackEnd FilePath -> Request -> ResourceT IO (Env m)
requestFormEnv backend req = do
#endif
	(query, files) <- parseRequestBody backend req
	return $ queryFormEnv (toQuery query ++ toQuery (FileQuery files) ++ queryString req)

-- | Build an 'Text.Digestive.Types.Env' from request body and query string
--
-- Uses a default temporary file 'Network.Wai.Parse.BackEnd'
#if MIN_VERSION_wai(2,0,0)
requestFormEnv_ :: (Monad m, MonadIO io) => Request -> ResourceT io (Env m)
requestFormEnv_ req = do
	st <- getInternalState
	requestFormEnv (tempFileBackEnd st) req
#else
requestFormEnv_ :: (Monad m) => Request -> ResourceT IO (Env m)
requestFormEnv_ = requestFormEnv tempFileBackEnd
#endif

-- | Format form paths just like PHP/Rails
pathToText :: [Text] -> Text
pathToText [] = T.empty
pathToText [p] = p
pathToText (p:ps)
	| T.null p = pathToText ps -- Eat empties off the front
	| otherwise = T.concat (p : concatMap fragment ps)
	where
	fragment n = [
			T.singleton '[',
			n,
			T.singleton ']'
		]
