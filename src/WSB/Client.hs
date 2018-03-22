{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-|
Module      : WSB client for Haskell
Description : Client library for the Web Service Billing
Copyright   : (c) josejuan, 2017
License     : GPL-3
Maintainer  : jose-juan@computer-mind.com
Stability   : experimental
Portability : POSIX

Client library for the Web Service Billing
-}
module WSB.Client (
  AuthReq(..)
, authReq
, CounterTypeReq(..)
, CounterReq(..)
, CounterTypeData(..)
, counterTypeList
, counterTypeAdd
, counterTypeRead
, counterTypeDelete
, counterRead
, counterPost
, counterReset
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.Hash.Algorithms
import Crypto.MAC.HMAC
import Data.Aeson
import Data.Monoid
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.ByteString.Conversion
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock.POSIX
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.HTTP.Types
import WSB.Client.Counter
import WSB.Client.CounterType
import WSB.CounterVersion
import WSB.Args
import qualified Crypto.MAC.HMAC as Crypto
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as LBS

epochTime :: IO Int
epochTime = round <$> getPOSIXTime

-- |Store the common configuration for all WSB operations
data AuthReq = AuthReq { authUserId   :: Int64      -- ^Your user id, you can find it on your WSB account profile
                       , authHashKey  :: ByteString -- ^Your HMAC secret key, you can find it on your WSB account profile
                       , authRequest  :: Request    -- ^The base `Request`, it will be used to override the `Method`, `RequestHeaders`, ...
                       } deriving (Show)

-- |Create one `AuthReq` with one `authUserId`, one `authHashKey` and the
-- WSB endpoint service
authReq :: MonadIO m => Int64 -> ByteString -> String -> m AuthReq
authReq userId authHashKey wsbURL = AuthReq userId authHashKey <$> liftIO (parseRequest wsbURL)

-- |Store the common configuration for all WSB operations related to one
-- counter type
data CounterTypeReq = CounterTypeReq { ctrAuth :: AuthReq   -- ^The base configuration
                                     , ctrCode :: Text      -- ^Your own counter type code
                                     } deriving (Show)

-- |Store the common configuration for all WSB operations related to ne
-- counter
data CounterReq = CounterReq { cCounterType :: CounterTypeReq -- ^The base configuration
                             , cCode        :: Text           -- ^Your own counter code
                             } deriving (Show)

-- |Information retrieved when one counter type is readed
data CounterTypeData = CounterTypeData { counterType :: CounterType -- ^The counter type information
                                       , counters    :: [Counter]   -- ^Counter type's counters
                                       } deriving (Show, Generic)

instance FromJSON CounterTypeData
instance ToJSON CounterTypeData

checkSuccess :: MonadIO m => Response LBS.ByteString -> ExceptT ByteString m ()
checkSuccess rs = unless (getResponseStatus rs == status200) $ throwError (LBS.toStrict $ getResponseBody rs)

httpReq :: MonadIO m => Method -> (Request, Args) -> ExceptT ByteString m (Response LBS.ByteString)
httpReq _method (request, Args userId counterTypeCode counterCode name value k1 k2 mode (Just time) (Just hmac)) = do
  rs <- httpLBS $ request { path = "/" <> toBS userId
                                <> maybe "" (("/" <>) . toBS) counterTypeCode
                                <> maybe "" (("/" <>) . toBS) counterCode
                          , method = _method
                          , requestHeaders = requestHeaders request
                                          ++ [("wsb-hmac", hmac)]
                                          ++ maybe [] ((:[]).("wsb-name" ,) . toBS) name
                                          ++ maybe [] ((:[]).("wsb-value",) . toBS) value
                                          ++ maybe [] ((:[]).("wsb-k1"   ,) . toBS) k1
                                          ++ maybe [] ((:[]).("wsb-k2"   ,) . toBS) k2
                                          ++ maybe [] ((:[]).("wsb-mode" ,) . toBS . show) mode
                                          ++ [("wsb-time" , toBS time)]
                          }
  checkSuccess rs
  return rs

jsonReq :: (FromJSON a, MonadIO m) => Method -> (Request, Args) -> ExceptT ByteString m a
jsonReq method args = do
  rs <- httpReq method args
  case eitherDecode' (getResponseBody rs) of
    Right v -> return v
    Left  e -> throwError $ toBS e

args :: MonadIO m => AuthReq -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int64 -> Maybe Int64 -> Maybe Int64 -> Maybe CounterVersion -> ExceptT ByteString m (Request, Args)
args (AuthReq userId skey request) counterTypeCode counterCode name value k1 k2 mode = do
  time <- Just <$> liftIO epochTime
  let a = Args userId counterTypeCode counterCode name value k1 k2 mode time Nothing
  return (request, a { argHMAC = Just $ computeHMAC a skey })

-- |Get all your counter types
counterTypeList :: MonadIO m => AuthReq -> ExceptT ByteString m [CounterType]
counterTypeList auth =
  args auth Nothing Nothing Nothing Nothing Nothing Nothing Nothing >>= jsonReq methodGet

-- |Read one counter type
counterTypeRead :: MonadIO m => CounterTypeReq -> ExceptT ByteString m CounterTypeData
counterTypeRead (CounterTypeReq auth ctrCode) =
  args auth (Just ctrCode) Nothing Nothing Nothing Nothing Nothing Nothing >>= jsonReq methodGet

-- |Add a new one counter type
counterTypeAdd :: MonadIO m => CounterTypeReq -> Text -> Int64 -> Int64 -> Int64 -> CounterVersion -> ExceptT ByteString m ()
counterTypeAdd (CounterTypeReq auth ctrCode) name value k1 k2 version =
  args auth (Just ctrCode) Nothing (Just name) (Just value) (Just k1) (Just k2) (Just version) >>= void . httpReq methodPut

-- |Delete one counter type and all their counters
counterTypeDelete :: MonadIO m => CounterTypeReq -> ExceptT ByteString m ()
counterTypeDelete (CounterTypeReq auth ctrCode) =
  args auth (Just ctrCode) Nothing Nothing Nothing Nothing Nothing Nothing >>= void . httpReq methodDelete

-- |Read the state of one counter
counterRead :: MonadIO m => CounterReq -> ExceptT ByteString m Counter
counterRead (CounterReq (CounterTypeReq auth ctrCode) cCode) =
  args auth (Just ctrCode) (Just cCode) Nothing Nothing Nothing Nothing Nothing >>= jsonReq methodGet

-- |Send a new value to the counter. The final behavior is defined by the
-- counter type of the counter
counterPost :: MonadIO m => CounterReq -> Int64 -> ExceptT ByteString m ()
counterPost (CounterReq (CounterTypeReq auth ctrCode) cCode) value =
  args auth (Just ctrCode) (Just cCode) Nothing (Just value) Nothing Nothing Nothing >>= void . httpReq methodPost

-- |Reset the counter to the default counter type values
counterReset :: MonadIO m => CounterReq -> ExceptT ByteString m ()
counterReset (CounterReq (CounterTypeReq auth ctrCode) cCode) =
  args auth (Just ctrCode) (Just cCode) Nothing Nothing Nothing Nothing Nothing >>= void . httpReq methodDelete

