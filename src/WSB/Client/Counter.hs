{-# LANGUAGE DeriveGeneric #-}
module WSB.Client.Counter where

import Data.Text (Text)
import Data.Aeson
import Data.Int (Int64)
import GHC.Generics

-- |Counter status information
data Counter = Counter { code     :: Text   -- ^Your own counter code
                       , value    :: Int64  -- ^The current counter accumulated value
                       , timeRef  :: Int64  -- ^The time reference (when apply)
                       } deriving (Show, Generic)

instance FromJSON Counter
instance ToJSON Counter


