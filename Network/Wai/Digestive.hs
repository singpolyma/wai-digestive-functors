-- | Helpers to bind 'digestive-functors' onto a 'wai' request
module Network.Wai.Digestive (queryFormEnv, bodyFormEnv, bodyFormEnv_, requestFormEnv, requestFormEnv_) where

import Control.Arrow (second)
import Text.Digestive (Env, FormInput(..))
import Network.HTTP.Types.QueryLike (QueryLike, toQuery, toQueryValue)
import Network.Wai (Request, queryString)
import Control.Monad.Trans.Resource (ResourceT)
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
bodyFormEnv :: BackEnd FilePath -> Request -> Env (ResourceT IO)
bodyFormEnv backend req pth = do
	(query, files) <- parseRequestBody backend req
	queryFormEnv (toQuery query ++ toQuery (FileQuery files)) pth

-- | Build an 'Text.Digestive.Types.Env' from a request body
--
-- Uses a default temporary file 'Network.Wai.Parse.BackEnd'
bodyFormEnv_ :: Request -> Env (ResourceT IO)
bodyFormEnv_ = bodyFormEnv tempFileBackEnd

-- | Build an 'Text.Digestive.Types.Env' from request body and query string
requestFormEnv :: BackEnd FilePath -> Request -> Env (ResourceT IO)
requestFormEnv backend req pth = do
	(query, files) <- parseRequestBody backend req
	queryFormEnv (toQuery query ++ toQuery (FileQuery files) ++ queryString req) pth

-- | Build an 'Text.Digestive.Types.Env' from request body and query string
--
-- Uses a default temporary file 'Network.Wai.Parse.BackEnd'
requestFormEnv_ :: Request -> Env (ResourceT IO)
requestFormEnv_ = requestFormEnv tempFileBackEnd

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
