{-# LANGUAGE DeriveGeneric #-}
module WSB.Client.CounterType where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics
import WSB.CounterVersion

-- |The counter type information
data CounterType = CounterType { code         :: Text           -- ^Your own counter type code
                               , name         :: Text           -- ^The counter type name
                               , version      :: CounterVersion -- ^The counter type version (define the behavior of the counter)
                               , value        :: Int64          -- ^The initial value for counters
                               , k1           :: Int64          -- ^Minimum value
                               , k2           :: Int64          -- ^Maximum value
                               } deriving (Show, Generic)

instance FromJSON CounterType
instance ToJSON CounterType


